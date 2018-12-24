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
;; 1. Scriptability: I live in the command line. I want a programmable
;;    alternative to `list-packages' for updating and installing packages.
;; 2. Flexibility: I want packages from sources other than ELPA. Primarily
;;    github, because certain plugins are out-of-date through official channels,
;;    have changed hands, have a superior fork, or simply aren't in any ELPA
;;    repo.
;; 3. Stability: I used Cask before this. It would error out with cyrptic errors
;;    depending on the version of Emacs I used and the alignment of the planets.
;;    No more.
;; 4. Performance: A minor point, but this system is lazy-loaded (more so if you
;;    byte-compile). Not having to initialize package.el (or check that your
;;    packages are installed) every time you start up Emacs affords us precious
;;    seconds.
;; 5. Simplicity: No Cask, no external dependencies (unless you count make),
;;    just Emacs. Arguably, my config is still over-complicated, but shhh, it's
;;    fine. Everything is fine.
;;
;; You should be able to use package.el commands without any conflicts.
;;
;; See core/autoload/packages.el for more functions.

(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

(defvar doom-core-packages '(persistent-soft use-package quelpa async)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(setq package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" doom-packages-dir)
      package-gnupghome-dir (expand-file-name "gpg" doom-packages-dir)
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/"))
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.

      ;; Don't track MELPA, we'll use package.el for that
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-verbose doom-debug-mode
      quelpa-dir (expand-file-name "quelpa" doom-packages-dir))

;; accommodate INSECURE setting
(unless gnutls-verify-error
  (dolist (archive package-archives)
    (setcdr archive (replace-regexp-in-string "^https://" "http://" (cdr archive) t nil))))


;;
;; Bootstrapper

(defun doom-initialize-packages (&optional force-p)
  "Ensures that Doom's package management system, package.el and quelpa are
initialized, and `doom-packages', `packages-alist' and `quelpa-cache' are
populated, if they aren't already.

If FORCE-P is non-nil, do it anyway.
If FORCE-P is 'internal, only (re)populate `doom-packages'.

Use this before any of package.el, quelpa or Doom's package management's API to
ensure all the necessary package metadata is initialized and available for
them."
  (with-temp-buffer ; prevent buffer-local settings from propagating
    (let ((load-prefer-newer t)) ; reduce stale code issues
      ;; package.el and quelpa handle themselves if their state changes during
      ;; the current session, but if you change an packages.el file in a module,
      ;; there's no non-trivial way to detect that, so we give you a way to
      ;; reload only doom-packages (by passing 'internal as FORCE-P).
      (unless (eq force-p 'internal)
        ;; `package-alist'
        (when (or force-p (not (bound-and-true-p package-alist)))
          (doom-ensure-packages-initialized 'force)
          (setq load-path (cl-remove-if-not #'file-directory-p load-path)))
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
        (cl-flet
            ((_load
              (lambda (file &optional noerror)
                (condition-case e
                    (load file noerror t t)
                  ((debug error)
                   (signal 'doom-package-error
                           (list (or (doom-module-from-path file)
                                     '(:private . packages))
                                 e)))))))
          (let ((doom-modules (doom-modules))
                (doom--stage 'packages)
                (noninteractive t))
            (setq doom-packages nil)
            (_load (expand-file-name "packages.el" doom-core-dir))
            ;; We load the private packages file twice to ensure disabled
            ;; packages are seen ASAP, and a second time to ensure privately
            ;; overridden packages are properly overwritten.
            (let ((private-packages (expand-file-name "packages.el" doom-private-dir)))
              (_load private-packages t)
              (cl-loop for key being the hash-keys of doom-modules
                       for path = (doom-module-path (car key) (cdr key) "packages.el")
                       do (let ((doom--current-module key)) (_load path t)))
              (_load private-packages t)
              (setq doom-packages (reverse doom-packages)))))))))


;;
;; Package API

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

(cl-defmacro package! (name &rest plist &key recipe pin disable _ignore _freeze)
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

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (doom--assert-stage-p 'packages #'package!)
  (let ((plist (append plist (cdr (assq name doom-packages)))))
    (when recipe
      (when (cl-evenp (length recipe))
        (setq plist (plist-put plist :recipe (cons name recipe))))
      (setq pin nil
            plist (plist-put plist :pin nil)))
    (when (file-in-directory-p (FILE!) doom-private-dir)
      (setq plist (plist-put plist :private t)))
    (let (newplist)
      (while plist
        (unless (null (cadr plist))
          (push (cadr plist) newplist)
          (push (car plist) newplist))
        (pop plist)
        (pop plist))
      (setq plist newplist))
    (macroexp-progn
     (append (if disable `((add-to-list 'doom-disabled-packages ',name nil #'eq)))
             (if pin `((setf (alist-get ',name package-pinned-packages) ,pin)))
             `((setf (alist-get ',name doom-packages) ',plist)
               (not (memq ',name doom-disabled-packages)))))))

(defmacro packages! (&rest packages)
  "A convenience macro like `package!', but allows you to declare multiple
packages at once.

Only use this macro in a module's packages.el file."
  (doom--assert-stage-p 'packages #'packages!)
  `(progn ,@(cl-loop for desc in packages collect `(package! ,@(doom-enlist desc)))))

(defmacro disable-packages! (&rest packages)
  "A convenience macro like `package!', but allows you to disable multiple
packages at once.

Only use this macro in a module's packages.el file."
  (doom--assert-stage-p 'packages #'disable-packages!)
  `(setq doom-disabled-packages (append ',packages doom-disabled-packages)))

(defmacro depends-on! (module submodule &optional flags)
  "Declares that this module depends on another.

Only use this macro in a module's packages.el file.

MODULE is a keyword, and SUBMODULE is a symbol. Under the hood, this simply
loads MODULE SUBMODULE's packages.el file."
  (doom--assert-stage-p 'packages #'depends-on!)
  `(let ((doom-modules ,doom-modules)
         (flags ,flags))
     (when flags
       (doom-module-put ,module ',submodule :flags flags))
     (load! "packages" ,(doom-module-locate-path module submodule) t)))

(provide 'core-packages)
;;; core-packages.el ends here
