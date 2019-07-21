;;; core/core-packages.el -*- lexical-binding: t; -*-

(require 'core-modules)

;; Emacs package management is opinionated, and so am I. I've bound together
;; `use-package', `quelpa' and package.el to create my own, rolling-release,
;; lazily-loaded package management system for Emacs.
;;
;; The three key commands are:
;;
;; + `bin/doom install`: Installs packages that are wanted, but not installed.
;; + `bin/doom update`: Updates packages that are out-of-date.
;; + `bin/doom autoremove`: Uninstalls packages that are no longer needed.
;;
;; This system reads packages.el files located in each activated module (and one
;; in `doom-core-dir'). These contain `package!' blocks that tell DOOM what
;; plugins to install and where from.
;;
;; Why all the trouble? Because:
;; 1. *Scriptability:* I live in the command line. I want a shell-scriptable
;;    interface for updating and installing Emacs packages.
;; 2. *Reach:* I want packages from sources other than ELPA (like github or
;;    gitlab). Some plugins are out-of-date through official channels, have
;;    changed hands, have a superior fork, or simply aren't available in ELPA
;;    repos.
;; 3. *Performance:* The package management system isn't loaded until you use
;;    the package management API. Not having to initialize package.el or quelpa
;;    (and check that your packages are installed) every time you start up (or
;;    load a package) speeds things up a great deal.
;; 4. *Separation of concerns:* It's more organized and reduces cognitive load
;;    to separate configuring of packages and installing/updating them.
;;
;; You should be able to use package.el commands without any conflicts.
;;
;; See core/autoload/packages.el for more functions.

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
  "A list of packages that should be ignored by `def-package!' and `after!'.")


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
(def-advice! doom--package-inhibit-custom-file-a (&optional value)
  :override #'package--save-selected-packages
  (if value (setq package-selected-packages value)))

;;; straight
(setq straight-cache-autoloads nil ; we already do this, and better.
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom refresh' instant (once everything set up), which is much
      ;; nicer UX than the several seconds modification checks add.
      straight-check-for-modifications nil
      ;; We do this ourselves, and a little more comprehensively.
      straight-enable-package-integration nil
      ;; Before switching to straight, `doom-local-dir' would average out at
      ;; around 100mb. Afterwards, at around 1gb. With shallow cloning, that is
      ;; reduced to ~400mb. This imposes an isuse with packages that require
      ;; their git history for certain things to work (like magit and org), but
      ;; we're prepared for that.
      straight-vc-git-default-clone-depth 1
      ;; Straight's own emacsmirror mirro is a little smaller and faster.
      straight-recipes-emacsmirror-use-mirror t
      ;; Prefix declarations are unneeded bulk added to our autoloads file. Best
      ;; we just don't have to deal with them at all.
      autoload-compute-prefixes nil)

;; Straight is hardcoded to operate out of ~/.emacs.d/straight. Not on my watch!
(def-advice! doom--straight-use-local-dir-a (orig-fn &rest args)
  :around #'straight--emacs-dir
  (let ((user-emacs-directory doom-local-dir))
    (apply orig-fn args)))


;;
;;; Bootstrapper

(defun doom-initialize-packages (&optional force-p)
  "Ensures that Doom's package system and straight.el are initialized.

If FORCE-P is non-nil, do it anyway.

This ensure `doom-packages' is populated, if isn't aren't already. Use this
before any of straight's or Doom's package management's API to ensure all the
necessary package metadata is initialized and available for them."
  (when (or force-p (not doom-init-packages-p))
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
      (add-hook 'kill-emacs-hook #'straight--transaction-finalize))
    (dolist (package (straight--directory-files (straight--build-dir)))
      (add-to-list 'load-path (directory-file-name (straight--build-dir package)))))
  (when (or force-p (not doom-packages))
    (setq doom-disabled-packages nil
          doom-packages (doom-package-list))
    (cl-loop for (pkg . plist) in doom-packages
             for ignored = (eval (plist-get plist :ignore) t)
             for disabled = (eval (plist-get plist :disable) t)
             if disabled
             do (add-to-list 'doom-disabled-packages pkg)
             else if (not ignored)
             do (with-demoted-errors "Package error: %s"
                  (straight-register-package
                   (if-let (recipe (plist-get plist :recipe))
                       `(,pkg ,@recipe)
                     pkg))))))

(defun doom-ensure-straight ()
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (defvar bootstrap-version)
  (let* ((straight-dir (expand-file-name "straight/" doom-local-dir))
         (bootstrap-file (expand-file-name "repos/straight.el/straight.el" straight-dir))
         (bootstrap-version 5)
         ;; Force straight to install into ~/.emacs.d/.local/straight instead of
         ;; ~/.emacs.d/straight by pretending `doom-local-dir' is our .emacs.d.
         (user-emacs-directory doom-local-dir))
    (cl-block 'straight
      ;; Straight will throw `emacs-version-changed' if it's loaded with a
      ;; version of Emacs that doesn't match the one it was compiled with.
      ;; Getting this error isn't very good UX...
      (catch 'emacs-version-changed
        (unless (require 'straight nil t)
          (unless (file-exists-p bootstrap-file)
            (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
              (goto-char (point-max))
              (eval-print-last-sexp)))
          (load bootstrap-file nil 'nomessage))
        (cl-return-from 'straight t))
      ;; ...so we transform it into a more graceful error message:
      (with-temp-buffer
        (insert-file-contents-literally (expand-file-name "build-cache.el" straight-dir))
        (let ((_ (read (current-buffer)))
              (last-emacs-version (read (current-buffer))))
          (user-error "Your version of Emacs has changed (from %S to %S). You must rebuild your packages with 'doom rebuild'."
                      emacs-version last-emacs-version))))))


;;
;;; Module package macros

(cl-defmacro package! (name &rest plist &key built-in _recipe disable ignore _freeze)
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
    (let ((module-list (plist-get old-plist :modules))
          (module (or doom--current-module
                      (let ((file (file!)))
                        (cond ((file-in-directory-p file doom-private-dir)
                               (list :private))
                              ((file-in-directory-p file doom-core-dir)
                               (list :core))
                              ((doom-module-from-path file)))))))
      (unless (member module module-list)
        (setq module-list (append module-list (list module) nil)
              plist (plist-put plist :modules module-list))))
    (when built-in
      (doom-log "Ignoring built-in package %S" name)
      (when (equal built-in '(quote prefer))
        (setq built-in `(locate-library ,(symbol-name name) nil doom--initial-load-path))))
    (setq plist (plist-put plist :ignore (or built-in ignore)))
    (while plist
      (unless (null (cadr plist))
        (setq old-plist (plist-put old-plist (car plist) (cadr plist))))
      (pop plist)
      (pop plist))
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
