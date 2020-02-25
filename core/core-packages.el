;;; core/core-packages.el -*- lexical-binding: t; -*-

;; Emacs package management is opinionated, and so is Doom. Doom uses `straight'
;; to create a declarative, lazy-loaded and optionally rolling-release package
;; management system. We use `straight' over `package' because the latter is
;; tempermental. ELPA sources suffer downtime occasionally, and often fail at
;; building some packages when GNU Tar is unavailable (e.g. MacOS users start
;; with BSD tar). There are also known gnutls errors in the current stable
;; release of Emacs (26.x) which bork TLS handshakes with ELPA repos (mainly
;; gnu.elpa.org). See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434.
;;
;; What's worse, you can only get the latest version of packages through ELPA.
;; In an ecosystem that is constantly changing, this is more frustrating than
;; convenient. Straight (and Doom) can do rolling release, but it is optional
;; (and will eventually be opt-in).
;;
;; ANyhow, interacting with this package management system is done through the
;; bin/doom script included with Doom Emacs. You'll find more about it by
;; running 'doom help' (I highly recommend you add it to your PATH), but here
;; are the highlights:
;;
;; + `bin/doom install`: a wizard that guides you through setting up Doom and
;;   your private config for the first time.
;; + `bin/doom sync`: your go-to command for making sure Doom is in optimal
;;   condition. It ensures all unneeded packages are removed, all needed ones
;;   are installed, and all metadata associated with them is generated.
;; + `bin/doom upgrade`: upgrades Doom Emacs and your packages to the latest
;;   versions. There's also 'bin/doom update' for updating only your packages.
;;
;; How this works is: the system reads packages.el files located in each
;; activated module, your private directory (`doom-private-dir'), and one in
;; `doom-core-dir'. These contain `package!' declarations that tell DOOM what
;; plugins to install and where from.
;;
;; All that said, you can still use package.el's commands, but 'bin/doom
;; refresh' will purge ELPA packages.

(defvar doom-init-packages-p nil
  "If non-nil, Doom's package management system has been initialized.")

(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

(defvar doom-core-packages '(straight use-package)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar doom-core-package-sources
  '((org-elpa :local-repo nil)
    (melpa
     :type git :host github
     :repo "melpa/melpa"
     :no-build t)
    (gnu-elpa-mirror
     :type git :host github
     :repo "emacs-straight/gnu-elpa-mirror"
     :no-build t)
    (emacsmirror-mirror
     :type git :host github
     :repo "emacs-straight/emacsmirror-mirror"
     :no-build t))
  "A list of recipes for straight's recipe repos.")

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")


;;
;;; Package managers

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quick package testing.
(setq package-enable-at-startup nil
      package-user-dir (concat doom-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        `(("gnu"   . ,(concat proto "://elpa.gnu.org/packages/"))
          ("melpa" . ,(concat proto "://melpa.org/packages/"))
          ("org"   . ,(concat proto "://orgmode.org/elpa/")))))

(advice-add #'package--ensure-init-file :override #'ignore)

;; Don't save `package-selected-packages' to `custom-file'
(defadvice! doom--package-inhibit-custom-file-a (&optional value)
  :override #'package--save-selected-packages
  (if value (setq package-selected-packages value)))

;; Refresh package.el the first time you call `package-install'
(add-transient-hook! 'package-install (package-refresh-contents))

;;; straight
(setq straight-base-dir doom-local-dir
      straight-repository-branch "develop"
      straight-cache-autoloads nil ; we already do this, and better.
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom refresh' instant (once everything set up), which is much
      ;; nicer UX than the several seconds modification checks.
      straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      straight-enable-package-integration nil
      ;; Before switching to straight, `doom-local-dir' would average out at
      ;; around 100mb with half Doom's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This
      ;; imposes an issue with packages that require their git history for
      ;; certain things to work (like magit and org), but we can deal with that
      ;; when we cross that bridge.
      straight-vc-git-default-clone-depth 1
      ;; Prefix declarations are unneeded bulk added to our autoloads file. Best
      ;; we just don't have to deal with them at all.
      autoload-compute-prefixes nil
      ;; We handle it ourselves
      straight-fix-org nil)

(defadvice! doom--read-pinned-packages-a (orig-fn &rest args)
  "Read from `doom-pinned-packages' on top of straight's lockfiles."
  :around #'straight--lockfile-read-all
  (append (apply orig-fn args)
          (doom-package-pinned-list)))


;;
;;; Bootstrapper

(defun doom-initialize-packages (&optional force-p)
  "Ensures that Doom's package system and straight.el are initialized.

If FORCE-P is non-nil, do it anyway.

This ensure `doom-packages' is populated, if isn't aren't already. Use this
before any of straight's or Doom's package management's API to ensure all the
necessary package metadata is initialized and available for them."
  (unless doom-init-packages-p
    (setq force-p t))
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (doom-log "Initializing package.el")
    (require 'package)
    (package-initialize))
  (when (or force-p (not doom-packages))
    (doom-log "Initializing straight")
    (setq doom-init-packages-p t)
    (doom-ensure-straight)
    (mapc #'straight-use-package doom-core-packages)
    (doom-log "Initializing doom-packages")
    (setq doom-disabled-packages nil
          doom-pinned-packages nil
          doom-packages (doom-package-list))
    (dolist (package doom-packages)
      (let ((name (car package)))
        (with-plist! (cdr package) (recipe modules disable ignore pin)
          (if ignore
              (doom-log "Ignoring package %S" name)
            (if (not disable)
                (with-demoted-errors "Package error: %s"
                  (when recipe
                    (straight-override-recipe (cons name recipe)))
                  (straight-register-package name))
              (doom-log "Disabling package %S" name)
              (cl-pushnew name doom-disabled-packages)
              ;; Warn about disabled core packages
              (when (cl-find :core modules :key #'car)
                (print! (warn "%s\n%s")
                        (format "You've disabled %S" name)
                        (indent 2 (concat "This is a core package. Disabling it will cause errors, as Doom assumes\n"
                                          "core packages are always available. Disable their minor-modes or hooks instead.")))))))))))

(defun doom-ensure-straight ()
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (unless (fboundp 'straight--reset-caches)
    (defvar bootstrap-version)
    (let* (;; Force straight to install into ~/.emacs.d/.local/straight instead of
           ;; ~/.emacs.d/straight by pretending `doom-local-dir' is our .emacs.d.
           (user-emacs-directory straight-base-dir)
           (bootstrap-file (doom-path straight-base-dir "straight/repos/straight.el/straight.el"))
           (bootstrap-version 5))
      (make-directory (doom-path straight-base-dir "straight/build") 'parents)
      (or (require 'straight nil t)
          (file-readable-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               (format "https://raw.githubusercontent.com/raxod502/straight.el/%s/install.el"
                       straight-repository-branch)
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil t))
    (require 'straight))
  (straight--reset-caches)
  (setq straight-recipe-repositories nil
        straight-recipe-overrides nil)
  (mapc #'straight-use-recipes doom-core-package-sources)
  (straight-register-package
   `(straight :type git :host github
              :repo ,(format "%s/straight.el" straight-repository-user)
              :files ("straight*.el")
              :branch ,straight-repository-branch
              :no-byte-compile t)))


;;
;;; Module package macros

(cl-defmacro package!
    (name &rest plist &key built-in recipe ignore _pin _disable)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `doom-packages' with metadata about the packages Doom needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :recipe RECIPE
   Specifies a straight.el recipe to allow you to acquire packages from external
   sources. See https://github.com/raxod502/straight.el#the-recipe-format for
   details on this recipe.
 :disable BOOL
   Do not install or update this package AND disable all of its `use-package!'
   and `after!' blocks.
 :ignore FORM
   Do not install this package.
 :pin STR|nil
   Pin this package to commit hash STR. Setting this to nil will unpin this
   package if previously pinned.
 :built-in BOOL|'prefer
   Same as :ignore if the package is a built-in Emacs package. This is more to
   inform help commands like `doom/help-packages' that this is a built-in
   package. If set to 'prefer, the package will not be installed if it is
   already provided by Emacs.

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (when (and recipe (keywordp (car-safe recipe)))
    (plist-put! plist :recipe `(quote ,recipe)))
  ;; :built-in t is basically an alias for :ignore (locate-library NAME)
  (when built-in
    (when (and (not ignore)
               (equal built-in '(quote prefer)))
      (setq built-in `(locate-library ,(symbol-name name) nil doom--initial-load-path)))
    (plist-delete! plist :built-in)
    (plist-put! plist :ignore built-in))
  `(let* ((name ',name)
          (plist (cdr (assq name doom-packages))))
     ;; Record what module this declaration was found in
     (let ((module-list (plist-get plist :modules))
           (module ',(doom-module-from-path)))
       (unless (member module module-list)
         (plist-put! plist :modules
                     (append module-list
                             (list module)
                             nil))))
     ;; Merge given plist with pre-existing one
     (doplist! ((prop val) (list ,@plist) plist)
       (unless (null val)
         (plist-put! plist prop val)))
     ;; Some basic key validation; error if you're not using a valid key
     (condition-case e
         (when-let (recipe (plist-get plist :recipe))
           (cl-destructuring-bind
               (&key local-repo _files _flavor
                     _no-build _no-byte-compile _no-autoloads
                     _type _repo _host _branch _remote _nonrecursive _fork _depth)
               recipe
             ;; Expand :local-repo from current directory
             (when local-repo
               (plist-put! plist :recipe
                           (plist-put recipe :local-repo
                                      (expand-file-name local-repo ,(dir!)))))))
       (error
        (signal 'doom-package-error
                (cons ,(symbol-name name)
                      (error-message-string e)))))
     ;; This is the only side-effect of this macro!
     (setf (alist-get name doom-packages) plist)
     (with-no-warnings
       (not (plist-get plist :disable)))))

(defmacro disable-packages! (&rest packages)
  "A convenience macro for disabling packages in bulk.
Only use this macro in a module's (or your private) packages.el file."
  (macroexp-progn
   (cl-loop for p in packages
            collect `(package! ,p :disable t))))

(defmacro unpin! (&rest targets)
  "Unpin packages in TARGETS.

This unpins packages, so that 'doom upgrade' downloads their latest version. It
can be used one of five ways:

+ To disable pinning wholesale: (unpin! t)
+ To unpin individual packages: (unpin! packageA packageB ...)
+ To unpin all packages in a group of modules: (unpin! :lang :tools ...)
+ To unpin packages in individual modules:
    (unpin! (:lang python javascript) (:tools docker))

Or any combination of the above.

This macro should only be used from the user's private packages.el. No module
should use it!"
  (if (memq t targets)
      `(mapc (doom-rpartial #'doom-package-set :unpin t)
             (mapcar #'car doom-packages))
    (let (forms)
      (dolist (target targets)
        (cl-check-type target (or symbol keyword list))
        (cond
         ((symbolp target)
          (push `(doom-package-set ',target :unpin t) forms))
         ((or (keywordp target)
              (listp target))
          (cl-destructuring-bind (category . modules) (doom-enlist target)
            (dolist (pkg doom-packages)
              (let ((pkg-modules (plist-get (cdr pkg) :modules)))
                (and (assq category pkg-modules)
                     (or (null modules)
                         (cl-loop for module in modules
                                  if (member (cons category module) pkg-modules)
                                  return t))
                     (push `(doom-package-set ',(car pkg) :unpin t) forms))))))))
      (macroexp-progn forms))))

(provide 'core-packages)
;;; core-packages.el ends here
