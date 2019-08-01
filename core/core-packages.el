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
;; + `bin/doom refresh`: your go-to command for making sure Doom is in optimal
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

(defvar doom-core-packages '(straight use-package async)
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
(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-user-dir doom-elpa-dir
      package-gnupghome-dir (expand-file-name "gpg" doom-elpa-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      (let ((proto (if gnutls-verify-error "http" "https")))
        `(("gnu"   . ,(concat proto "://elpa.gnu.org/packages/"))
          ("melpa" . ,(concat proto "://melpa.org/packages/"))
          ("org"   . ,(concat proto "://orgmode.org/elpa/")))))

;; Don't save `package-selected-packages' to `custom-file'
(defadvice! doom--package-inhibit-custom-file-a (&optional value)
  :override #'package--save-selected-packages
  (if value (setq package-selected-packages value)))

;;; straight
(setq straight-cache-autoloads nil ; we already do this, and better.
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
      ;; Straight's own emacsmirror mirror is a little smaller and faster.
      straight-recipes-emacsmirror-use-mirror t
      ;; Prefix declarations are unneeded bulk added to our autoloads file. Best
      ;; we just don't have to deal with them at all.
      autoload-compute-prefixes nil)

;; Straight is hardcoded to operate out of ~/.emacs.d/straight. Not on my watch!
(defadvice! doom--straight-use-local-dir-a (orig-fn &rest args)
  :around #'straight--emacs-dir
  (let ((user-emacs-directory doom-local-dir))
    (apply orig-fn args)))

(defun doom--finalize-straight ()
  (mapc #'funcall (delq nil (mapcar #'cdr straight--transaction-alist)))
  (setq straight--transaction-alist nil))


;;
;;; Bootstrapper

(defun doom-initialize-packages (&optional force-p)
  "Ensures that Doom's package system and straight.el are initialized.

If FORCE-P is non-nil, do it anyway.

This ensure `doom-packages' is populated, if isn't aren't already. Use this
before any of straight's or Doom's package management's API to ensure all the
necessary package metadata is initialized and available for them."
  (when (or force-p (not doom-init-packages-p))
    (doom-log "Initializing straight")
    (setq doom-init-packages-p t)
    (straight--reset-caches)
    (mapc #'straight-use-recipes doom-core-package-sources)
    (straight-register-package
     `(straight :type git :host github
                :repo ,(format "%s/straight.el" straight-repository-user)
                :files ("straight*.el")
                :branch ,straight-repository-branch))
    (mapc #'straight-use-package doom-core-packages)
    (when noninteractive
      (add-hook 'kill-emacs-hook #'doom--finalize-straight))
    (dolist (package (straight--directory-files (straight--build-dir)))
      (cl-pushnew (directory-file-name (straight--build-dir package))
                  load-path)))
  (when (or force-p (not doom-packages))
    (doom-log "Initializing doom-packages")
    (setq doom-disabled-packages nil
          doom-packages (doom-package-list))
    (cl-loop for (pkg . plist) in doom-packages
             for ignored = (eval (plist-get plist :ignore) t)
             for disabled = (eval (plist-get plist :disable) t)
             if disabled
             do (cl-pushnew pkg doom-disabled-packages)
             else if (not ignored)
             do (with-demoted-errors "Package error: %s"
                  (straight-register-package
                   (if-let* ((recipe (plist-get plist :recipe)))
                       (let ((plist (straight-recipes-retrieve pkg)))
                         `(,pkg ,@(doom-plist-merge recipe (cdr plist))))
                     pkg))))))

(defun doom-ensure-straight ()
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (defvar bootstrap-version)
  (let* (;; Force straight to install into ~/.emacs.d/.local/straight instead of
         ;; ~/.emacs.d/straight by pretending `doom-local-dir' is our .emacs.d.
         (user-emacs-directory doom-local-dir)
         (straight-dir   (doom-path doom-local-dir "straight/"))
         (build-file     (doom-path straight-dir "build/straight/straight.elc"))
         (bootstrap-file (doom-path straight-dir "repos/straight.el/straight.el"))
         (bootstrap-version 5))
    (cl-block nil
      ;; Straight will throw `emacs-version-changed' if it's loaded with a
      ;; version of Emacs that doesn't match the one it was compiled with.
      ;; Getting this error isn't very good UX...
      (catch 'emacs-version-changed
        (unless (featurep 'staight)
          (unless (or (load build-file 'noerror 'nomessage)
                      (load bootstrap-file 'noerror 'nomessage))
            (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
              (goto-char (point-max))
              (eval-print-last-sexp))
            (load bootstrap-file nil 'nomessage)))
        (cl-return t))
      ;; Get rid of old build files
      (when-let* ((build-dir (file-exists-p! "build/straight" straight-dir)))
        (delete-directory build-dir 'recursive))
      ;; Then transform the error into a more graceful failure message:
      (with-temp-buffer
        (insert-file-contents-literally (doom-path straight-dir "build-cache.el"))
        (let ((_ (read (current-buffer)))
              (last-emacs-version (read (current-buffer))))
          (user-error "Your version of Emacs has changed (from %S to %S). You must rebuild your packages with 'doom rebuild -f'."
                      emacs-version last-emacs-version))))))


;;
;;; Module package macros

(cl-defmacro package!
    (name &rest plist &key built-in _recipe disable ignore _freeze)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `doom-packages' with metadata about the packages Doom needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :recipe RECIPE
   Takes a MELPA-style recipe (see `quelpa-recipe' in `quelpa' for an example);
   for packages to be installed from external sources.
 :disable BOOL
   Do not install or update this package AND disable all of its `def-package!'
   blocks.
 :ignore FORM
   Do not install this package.
 :freeze FORM
   Do not update this package if FORM is non-nil.
 :built-in BOOL
   Same as :ignore if the package is a built-in Emacs package. If set to
   'prefer, will use built-in package if it is present.

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (let ((old-plist (cdr (assq name doom-packages))))
    ;; Add current module to :modules
    (let ((module-list (plist-get old-plist :modules))
          (module (doom-module-from-path)))
      (unless (member module module-list)
        (plist-put! plist :modules
                    (append module-list
                            (list module)
                            nil))))

    ;; Handle :built-in
    (unless ignore
      (when built-in
        (doom-log "Ignoring built-in package %S" name)
        (when (equal built-in '(quote prefer))
          (setq built-in `(locate-library ,(symbol-name name) nil doom--initial-load-path))))
      (plist-put! plist :ignore built-in))

    ;; DEPRECATED Translate :fetcher to :host
    (with-plist! plist (recipe)
      (with-plist! recipe (fetcher)
        (when fetcher
          (message "%s\n%s"
                   (format "WARNING: The :fetcher property was used for the %S package."
                           name)
                   "This property is deprecated. Replace it with :host.")
          (plist-put! recipe :host fetcher)
          (plist-delete! recipe :fetcher))
        (plist-put! plist :recipe recipe)))

    (doplist! ((prop val) plist)
      (unless (null val)
        (plist-put! old-plist prop val)))
    (setq plist old-plist)

    ;; TODO Add `straight-use-package-pre-build-function' support
    (macroexp-progn
     (append `((setf (alist-get ',name doom-packages) ',plist))
             (when disable
               `((doom-log "Disabling package %S" ',name)
                 (add-to-list 'doom-disabled-packages ',name nil 'eq)
                 nil))))))

(defmacro disable-packages! (&rest packages)
  "A convenience macro for disabling packages in bulk.
Only use this macro in a module's (or your private) packages.el file."
  (macroexp-progn
   (cl-loop for p in packages
            collect `(package! ,p :disable t))))

(provide 'core-packages)
;;; core-packages.el ends here
