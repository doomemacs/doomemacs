;;; core/core-packages.el -*- lexical-binding: t; -*-

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

(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

(defvar doom-core-packages
  '(persistent-soft use-package quelpa async)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `def-package!' and `after!'.")

;;; package.el
(setq package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" doom-packages-dir)
      package-gnupghome-dir (expand-file-name "gpg" doom-packages-dir)
      package-enable-at-startup nil
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      `(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("melpa" . -1)
        ("melpa-mirror" . -2)
        ("gnu" . -3)))

(when (or (not gnutls-verify-error)
          (not (ignore-errors (gnutls-available-p))))
  (dolist (archive package-archives)
    (setcdr archive (replace-regexp-in-string "^https://" "http://" (cdr archive) t nil))))

;;; quelpa
(setq quelpa-dir (expand-file-name "quelpa" doom-packages-dir)
      quelpa-verbose doom-debug-mode

      ;; Don't track MELPA, we'll use package.el for that
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil)


;;
;;; Bootstrapper

(defun doom-initialize-packages (&optional force-p)
  "Ensures that Doom's package management system, package.el and quelpa are
initialized, and `doom-packages', `packages-alist' and `quelpa-cache' are
populated, if they aren't already.

If FORCE-P is non-nil, do it anyway.
If FORCE-P is 'internal, only (re)populate `doom-packages'.

Use this before any of package.el, quelpa or Doom's package management's API to
ensure all the necessary package metadata is initialized and available for
them."
  (let ((load-prefer-newer t)) ; reduce stale code issues
    ;; package.el and quelpa handle themselves if their state changes during the
    ;; current session, but if you change a packages.el file in a module,
    ;; there's no non-trivial way to detect that, so to reload only
    ;; `doom-packages' pass 'internal as FORCE-P or use `doom/reload-packages'.
    (unless (eq force-p 'internal)
      ;; `package-alist'
      (when (or force-p (not (bound-and-true-p package-alist)))
        (doom-ensure-packages-initialized 'force)
        (setq load-path (cl-delete-if-not #'file-directory-p load-path)))
      ;; `quelpa-cache'
      (when (or force-p (not (bound-and-true-p quelpa-cache)))
        ;; ensure un-byte-compiled version of quelpa is loaded
        (unless (featurep 'quelpa)
          (load (locate-library "quelpa.el") nil t t))
        (setq quelpa-initialized-p nil)
        (or (quelpa-setup-p)
            (error "Could not initialize quelpa"))))
    ;; `doom-packages'
    (when (or force-p (not doom-packages))
      (setq doom-packages (doom-package-list)))))


;;
;;; Package API

(defun doom-ensure-packages-initialized (&optional force-p)
  "Make sure package.el is initialized."
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (require 'package)
    (setq package-activated-list nil
          package--initialized nil)
    (let (byte-compile-warnings)
      (condition-case _
          (package-initialize)
        ('error (package-refresh-contents)
                (setq doom--refreshed-p t)
                (package-initialize))))))

(defun doom-ensure-core-packages ()
  "Make sure `doom-core-packages' are installed."
  (when-let* ((core-packages (cl-remove-if #'package-installed-p doom-core-packages)))
    (message "Installing core packages")
    (unless doom--refreshed-p
      (package-refresh-contents))
    (dolist (package core-packages)
      (let ((inhibit-message t))
        (package-install package))
      (if (package-installed-p package)
          (message "✓ Installed %s" package)
        (error "✕ Couldn't install %s" package)))
    (message "Installing core packages...done")))


;;
;; Module package macros

(cl-defmacro package! (name &rest plist &key built-in recipe pin disable _ignore _freeze)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `doom-packages' with metadata about the packages Doom needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :recipe RECIPE
   Takes a MELPA-style recipe (see `quelpa-recipe' in `quelpa' for an example);
   for packages to be installed from external sources.
 :pin ARCHIVE-NAME
   Instructs ELPA to only look for this package in ARCHIVE-NAME. e.g. \"org\".
   Ignored if RECIPE is present.
 :disable BOOL
   Do not install or update this package AND disable all of its `def-package!'
   blocks.
 :ignore FORM
   Do not install this package.
 :freeze FORM
   Do not update this package if FORM is non-nil.
 :built-in BOOL
   Same as :ignore if the package is a built-in Emacs package.

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (doom--assert-stage-p 'packages #'package!)
  (let ((old-plist (cdr (assq name doom-packages))))
    (when recipe
      (when (cl-evenp (length recipe))
        (setq plist (plist-put plist :recipe (cons name recipe))))
      (setq pin nil
            plist (plist-put plist :pin nil)))
    (let ((module-list (plist-get old-plist :modules))
          (module (or doom--current-module
                      (let ((file (FILE!)))
                        (cond ((file-in-directory-p file doom-private-dir)
                               (list :private))
                              ((file-in-directory-p file doom-core-dir)
                               (list :core))
                              ((doom-module-from-path file)))))))
      (unless (member module module-list)
        (setq module-list (append module-list (list module) nil)
              plist (plist-put plist :modules module-list))))
    (when (and built-in (locate-library (symbol-name name) nil doom-site-load-path))
      (doom-log "Ignoring built-in package '%s'" name)
      (setq plist (plist-put plist :ignore t)))
    (while plist
      (unless (null (cadr plist))
        (setq old-plist (plist-put old-plist (car plist) (cadr plist))))
      (pop plist)
      (pop plist))
    (setq plist old-plist)
    (macroexp-progn
     (append (when pin
               (doom-log "Pinning package '%s' to '%s'" name pin)
               `((setf (alist-get ',name package-pinned-packages) ,pin)))
             `((setf (alist-get ',name doom-packages) ',plist))
             (when disable
               (doom-log "Disabling package '%s'" name)
               `((add-to-list 'doom-disabled-packages ',name nil 'eq)
                 nil))))))

(defmacro packages! (&rest packages)
  "A convenience macro for `package!' for declaring multiple packages at once.

Only use this macro in a module's packages.el file."
  (doom--assert-stage-p 'packages #'packages!)
  (macroexp-progn
   (cl-loop for desc in packages
            collect (macroexpand `(package! ,@(doom-enlist desc))))))

(defmacro disable-packages! (&rest packages)
  "A convenience macro like `package!', but allows you to disable multiple
packages at once.

Only use this macro in a module's packages.el file."
  (doom--assert-stage-p 'packages #'disable-packages!)
  (macroexp-progn
   (cl-loop for pkg in packages
            collect (macroexpand `(package! ,pkg :disable t)))))

(provide 'core-packages)
;;; core-packages.el ends here
