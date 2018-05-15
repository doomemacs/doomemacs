;;; core-packages.el --- package management system -*- lexical-binding: t; -*-

;; Emacs package management is opinionated. Unfortunately, so am I. I've bound
;; together `use-package', `quelpa' and package.el to create my own,
;; rolling-release, lazily-loaded package management system for Emacs.
;;
;; The three key commands are:
;;
;; + `make install` or `doom//packages-install': Installs packages that are
;;   wanted, but not installed.
;; + `make update` or `doom//packages-update': Updates packages that are
;;   out-of-date.
;; + `make autoremove` or `doom//packages-autoremove': Uninstalls packages that
;;   are no longer needed.
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

(defvar doom-init-p nil
  "Non-nil if doom is done initializing (once `doom-post-init-hook' is done). If
this is nil after Emacs has started something is wrong.")

(defvar doom-init-time nil
  "The time it took, in seconds, for DOOM Emacs to initialize.")

(defvar doom-modules
  (make-hash-table :test #'equal :size 100 :rehash-threshold 1.0)
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-modules-dirs
  (list (expand-file-name "modules/" doom-private-dir) doom-modules-dir)
  "A list of module root directories. Order determines priority.")

(defvar doom-psuedo-module-dirs
  (list doom-private-dir)
  "Additional paths for modules that are outside of `doom-modules-dirs'.
`doom//reload-autoloads', `doom//byte-compile' and `doom-initialize-packages'
will include the directories in this list.")

(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

(defvar doom-core-packages
  '(persistent-soft use-package quelpa async)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defvar doom-reload-hook nil
  "A list of hooks to run when `doom/reload-load-path' is called.")

(defvar doom-site-load-path load-path
  "The starting load-path, before it is altered by `doom-initialize'.")

(defvar doom-autoload-file (concat doom-local-dir "autoloads.el")
  "Where `doom//reload-autoloads' will generate its autoloads file.")

(defvar doom-packages-file (concat doom-cache-dir "packages.el")
  "Where to cache `load-path', `Info-directory-list', `doom-disabled-packages'
and `auto-mode-alist'.")

(defvar doom--current-module nil)
(defvar doom--refreshed-p nil)
(defvar doom--stage 'init)

;;
(setq autoload-compute-prefixes nil
      package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" doom-packages-dir)
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/"))
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.

      ;; security settings
      gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

      use-package-verbose doom-debug-mode
      use-package-minimum-reported-time (if doom-debug-mode 0 0.1)

      ;; Don't track MELPA, we'll use package.el for that
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-verbose doom-debug-mode
      quelpa-dir (expand-file-name "quelpa" doom-packages-dir)

      byte-compile-dynamic nil
      byte-compile-verbose doom-debug-mode
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; accommodate INSECURE setting
(unless gnutls-verify-error
  (dolist (archive package-archives)
    (setcdr archive (replace-regexp-in-string "^https://" "http://" (cdr archive) t nil))))


;;
;; Helpers 'n hooks
;;

(defun doom--assert-stage-p (stage macro)
  (cl-assert (eq stage doom--stage)
             nil
             "Found %s call in non-%s.el file (%s)"
             macro (symbol-name stage)
             (if (file-in-directory-p load-file-name doom-emacs-dir)
                 (file-relative-name load-file-name doom-emacs-dir)
               (abbreviate-file-name load-file-name))))

(defun doom|refresh-cache ()
  "Refresh `doom-packages-file', which caches `load-path',
`Info-directory-list', `doom-disabled-packages', `auto-mode-alist' and
`package-activated-list'."
  (doom-initialize-packages 'internal)
  (let ((coding-system-for-write 'emacs-internal))
    (with-temp-file doom-packages-file
      (insert ";;; -*- lexical-binding:t -*-\n"
              ";; This file was autogenerated by `doom|refresh-cache', DO NOT EDIT!\n")
      (prin1 `(setq load-path ',load-path
                    Info-directory-list ',Info-directory-list
                    auto-mode-alist ',auto-mode-alist
                    interpreter-mode-alist ',interpreter-mode-alist
                    doom-disabled-packages ',doom-disabled-packages
                    package-activated-list ',package-activated-list)
             (current-buffer)))))

(defun doom|display-benchmark (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Doom loaded %s packages across %d modules in %.03fs"
           ;; Certainly imprecise, especially where custom additions to
           ;; load-path are concerned, but I don't mind a [small] margin of
           ;; error in the plugin count in exchange for faster startup.
           (- (length load-path) (length doom-site-load-path))
           (hash-table-count doom-modules)
           (or doom-init-time
               (setq doom-init-time (float-time (time-subtract (current-time) before-init-time))))))

(add-hook 'emacs-startup-hook #'doom|display-benchmark)
(add-hook 'doom-reload-hook   #'doom|display-benchmark)


;;
;; Bootstrap API
;;

(defun doom-initialize (&optional force-p)
  "Bootstrap the bare essentials to get Doom running, if it hasn't already. If
FORCE-P is non-nil, do it anyway.

1. Ensures all the essential directories exist,
2. Ensures core packages are installed,
3. Loads your autoloads file in `doom-autoload-file',
4. Builds and caches `load-path', `Info-directory-list' and
   `doom-disabled-packages' in `doom-packages-file'"
  ;; Called early during initialization; only use native (and cl-lib) functions!
  (let ((load-path doom-site-load-path))
    (require 'subr-x)
    (require 'cl-lib)
    (require 'map))
  (when (or force-p (not doom-init-p))
    ;; autoloads file
    (unless (load doom-autoload-file 'noerror 'nomessage 'nosuffix)
      (unless noninteractive
        (error "No autoloads file! Run make autoloads")))
    ;; packages.el cache
    (when (and force-p (file-exists-p doom-packages-file))
      (delete-file doom-packages-file))
    (unless (load doom-packages-file 'noerror 'nomessage 'nosuffix)
      ;; Ensure core folders exist, otherwise we get errors
      (dolist (dir (list doom-local-dir doom-etc-dir doom-cache-dir doom-packages-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      ;; Ensure packages have been initialized
      (require 'package)
      (setq package-activated-list nil
            package--initialized nil)
      (let (byte-compile-warnings)
        (condition-case _
            (package-initialize)
          ('error (package-refresh-contents)
                  (setq doom--refreshed-p t)
                  (package-initialize))))
      ;; Ensure core packages are installed.
      (let ((core-packages (cl-remove-if #'package-installed-p doom-core-packages)))
        (when core-packages
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
      (cl-pushnew doom-core-dir load-path :test #'string=)
      (add-hook 'doom-internal-init-hook #'doom|refresh-cache))
    (when doom-debug-mode
      (message "Doom initialized"))
    (setq doom-init-p t))
  ;; initialize Doom core
  (require 'core-lib)
  (require 'core-os)
  (unless noninteractive
    (require 'core-ui)
    (require 'core-editor)
    (require 'core-projects)
    (require 'core-keybinds)))

(defun doom-initialize-autoloads ()
  "Ensures that `doom-autoload-file' exists and is loaded. Otherwise run
`doom//reload-autoloads' to generate it. Used from Doom's Makefile."
  (unless (file-exists-p doom-autoload-file)
    (quiet! (doom//reload-autoloads))))

(defun doom-initialize-packages (&optional force-p)
  "Ensures that `doom-packages', `packages-alist' and `quelpa-cache' are
populated.

This reads modules' packages.el files, runs `package-initialize', and
initializes quelpa, if they haven't already. If FORCE-P is non-nil, do it
anyway. If FORCE-P is 'internal, only (re)populate `doom-packages'.

Use this before any of package.el, quelpa or Doom's package management's API to
ensure all the necessary package metadata is initialized and available for
them."
  (with-temp-buffer ; prevent buffer-local settings from propagating
    (let ((load-prefer-newer t))
      ;; package.el and quelpa handle themselves if their state changes during
      ;; the current session, but if you change an packages.el file in a module,
      ;; there's no non-trivial way to detect that, so we give you a way to
      ;; reload only doom-packages (by passing 'internal as FORCE-P).
      ;; `doom-packages'
      (when (or force-p (not doom-packages))
        (unless (eq force-p 'internal)
          ;; `package-alist'
          (when (or force-p (not (bound-and-true-p package-alist)))
            (setq load-path doom-site-load-path)
            (require 'package)
            (setq package-activated-list nil
                  package--initialized nil)
            (let (byte-compile-warnings)
              (condition-case _
                  (package-initialize)
                ('error (package-refresh-contents)
                        (setq doom--refreshed-p t)
                        (package-initialize)))))

          ;; `quelpa-cache'
          (when (or force-p (not (bound-and-true-p quelpa-cache)))
            (require 'quelpa)
            (setq quelpa-initialized-p nil)
            (or (quelpa-setup-p)
                (error "Could not initialize quelpa"))))

        (setq doom-packages nil)
        (cl-flet
            ((_load
              (file &optional noerror interactive)
              (condition-case-unless-debug ex
                  (let ((noninteractive (not interactive)))
                    (load file noerror 'nomessage 'nosuffix))
                ('error
                 (lwarn 'doom-initialize-packages :warning
                        "%s in %s: %s"
                        (car ex)
                        (file-relative-name file doom-emacs-dir)
                        (error-message-string ex))))))
          (let ((doom--stage 'packages))
            (_load (expand-file-name "packages.el" doom-core-dir))
            (cl-loop for key being the hash-keys of doom-modules
                     for path = (doom-module-expand-file (car key) (cdr key) "packages.el")
                     if (file-exists-p path)
                     do (let ((doom--current-module key)) (_load path)))
            (cl-loop for dir in doom-psuedo-module-dirs
                     for path = (expand-file-name "packages.el" dir)
                     if (file-exists-p path)
                     do (_load path))))))))


;;
;; Module API
;;

(defun doom-module-p (module submodule)
  "Returns t if MODULE SUBMODULE is enabled (ie. present in `doom-modules')."
  (and (hash-table-p doom-modules)
       (gethash (cons module submodule) doom-modules)
       t))

(defun doom-module-get (module submodule &optional property)
  "Returns the plist for MODULE/SUBMODULE. If PROPERTY is set, get its property."
  (when-let* ((plist (gethash (cons module submodule) doom-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun doom-module-put (module submodule property value)
  "Set a PROPERTY for MODULE SUBMODULE to VALUE."
  (when-let* ((plist (doom-module-get module submodule)))
    (puthash (cons module submodule)
             (plist-put plist property value)
             doom-modules)))

(defun doom-module-set (module submodule &rest plist)
  "Adds MODULE and SUBMODULE to `doom-modules' and sets its plist to PLIST,
which should contain a minimum of :flags and :path.

MODULE is a keyword, SUBMODULE is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled module flags
  :path  [STRING]       path to module root directory

Example:

  (doom-module-set :lang 'haskell :flags '(+intero))

Used by `require!'."
  (when plist
    (let ((old-plist (doom-module-get module submodule)))
      (unless (plist-member plist :flags)
        (plist-put plist :flags (plist-get old-plist :flags)))
      (unless (plist-member plist :path)
        (plist-put plist :path (or (plist-get old-plist :path)
                                   (doom-module-find-path module submodule))))))
  (let ((key (cons module submodule)))
    (puthash key plist doom-modules)))

(defun doom-module-find-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (when (keywordp module)
    (setq module (substring (symbol-name module) 1)))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (cl-loop for default-directory in doom-modules-dirs
           for path = (concat module "/" submodule "/" file)
           if (file-exists-p path)
           return (expand-file-name path)))

(defun doom-module-from-path (&optional path)
  "Get module cons cell (MODULE . SUBMODULE) for PATH, if possible."
    (or doom--current-module
        (save-match-data
          (setq path (file-truename path))
          (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)/.*$" path)
            (when-let* ((module (match-string 1 path))
                        (submodule (match-string 2 path)))
              (cons (intern (concat ":" module))
                    (intern submodule)))))))

(defun doom-module-expand-file (module submodule &optional file)
  "Like `expand-file-name', but expands FILE relative to MODULE (keywordp) and
SUBMODULE (symbol)"
  (let ((path (doom-module-get module submodule :path)))
    (if file
        (expand-file-name file path)
      path)))

(defun doom-module-load-path ()
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added, if the file exists."
  (append (cl-loop for plist being the hash-values of doom-modules
                   collect (plist-get plist :path))
          (cl-remove-if-not #'file-directory-p doom-psuedo-module-dirs)))


;;
;; Module config macros
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro doom! (&rest modules)
  "Bootstraps DOOM Emacs and its modules.

MODULES is an malformed plist of modules to load."
  (let (init-forms config-forms file-name-handler-alist)
    (let (module)
      (dolist (m modules)
        (cond ((keywordp m) (setq module m))
              ((not module) (error "No namespace specified in `doom!' for %s" m))
              ((let ((submodule (if (listp m) (car m) m))
                     (flags     (if (listp m) (cdr m))))
                 (let ((path (doom-module-find-path module submodule)))
                   (if (not path)
                       (when doom-debug-mode
                         (message "Couldn't find the %s %s module" module submodule))
                     (doom-module-set module submodule :flags flags :path path)
                     (push `(let ((doom--current-module ',(cons module submodule)))
                              (load! init ,path t))
                           init-forms)
                     (push `(let ((doom--current-module ',(cons module submodule)))
                              (load! config ,path t))
                           config-forms))))))))
    `(let (file-name-handler-alist)
       (setq doom-modules ',doom-modules)
       ,@(nreverse init-forms)
       (unless noninteractive
         (run-hooks 'doom-internal-init-hook)
         (let ((doom--stage 'config))
           ,@(nreverse config-forms)
           (when doom-private-dir
             (load ,(concat doom-private-dir "config") t t)))))))

(defmacro def-package! (name &rest plist)
  "A thin wrapper around `use-package'."
  ;; Ignore package if NAME is in `doom-disabled-packages'
  (when (and (memq name doom-disabled-packages)
             (not (memq :disabled plist)))
    (setq plist `(:disabled t ,@plist)))
  ;; If byte-compiling, ignore this package if it doesn't meet the condition.
  ;; This avoids false-positive load errors.
  (unless (and (bound-and-true-p byte-compile-current-file)
               (or (and (plist-member plist :if)     (not (eval (plist-get plist :if) t)))
                   (and (plist-member plist :when)   (not (eval (plist-get plist :when) t)))
                   (and (plist-member plist :unless) (eval (plist-get plist :unless) t))))
    `(progn
       ,(when-let* ((defer (plist-get plist :defer))
                    (value (or (car-safe defer) defer)))
          (setq plist (plist-put plist :defer (or (cdr-safe defer) t)))
          (unless (or (memq value '(t nil))
                      (number-or-marker-p value))
            `(add-transient-hook! ',value
               ,(intern (format "load-%s" name))
               (require ',name))))
       (use-package ,name ,@plist))))

(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

Only use this macro in a module's init.el file.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config

WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Doom's config)."
  (declare (indent defun))
  (doom--assert-stage-p 'init #'package!)
  (cond ((eq when :disable)
         (message "Using :disable with `def-package-hook!' is deprecated. Use :disable in `package!' instead.")
         (ignore (push package doom-disabled-packages)))
        ((memq when '(:pre-init :post-init :pre-config :post-config))
         `(progn
            (setq use-package-inject-hooks t)
            (add-hook!
              ',(intern (format "use-package--%s--%s-hook"
                                package
                                (substring (symbol-name when) 1)))
              ,@body)))
        (t
         (error "'%s' isn't a valid hook for def-package-hook!" when))))

(defmacro load! (filesym &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILESYM is either a symbol or string representing the file to load. PATH is
where to look for the file (a string representing a directory path). If omitted,
the lookup is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (or (symbolp filesym)
      (signal 'wrong-type-argument (list 'symbolp filesym)))
  (let ((path (or path
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  (error "Could not detect path to look for '%s' in" filesym)))
        (filename (symbol-name filesym)))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror
                 ,(not doom-debug-mode))
        (unless noerror
          (error "Could not load file '%s' from '%s'" file path))))))

(defmacro require! (module submodule &optional reload-p &rest plist)
  "Loads the module specified by MODULE (a property) and SUBMODULE (a symbol).

The module is only loaded once. If RELOAD-P is non-nil, load it again."
  (let ((enabled-p (doom-module-p module submodule)))
    (when (or (not enabled-p) plist)
      (apply #'doom-module-set module submodule
             (mapcar #'eval plist)))
    (when (or reload-p (not enabled-p))
      (let ((module-path (doom-module-find-path module submodule)))
        (if (file-directory-p module-path)
            `(condition-case-unless-debug ex
                 (let ((doom--current-module ',(cons module submodule)))
                   (load! init ,module-path :noerror)
                   (let ((doom--stage 'config))
                     (load! config ,module-path :noerror)))
               ('error
                (lwarn 'doom-modules :error
                       "%s in '%s %s' -> %s"
                       (car ex) ,module ',submodule
                       (error-message-string ex))))
          (warn 'doom-modules :warning "Couldn't find module '%s %s'"
                module submodule))))))

(defmacro featurep! (module &optional submodule flag)
  "Returns t if MODULE SUBMODULE is enabled. If FLAG is provided, returns t if
MODULE SUBMODULE has FLAG enabled.

  (featurep! :config default)

Module FLAGs are set in your config's `doom!' block, typically in
~/.emacs.d/init.el. Like so:

  :config (default +flag1 -flag2)

When this macro is used from inside a module, MODULE and SUBMODULE can be
omitted. eg. (featurep! +flag1)"
  (unless submodule
    (let* ((path (or load-file-name byte-compile-current-file))
           (module-pair (doom-module-from-path path)))
      (unless module-pair
        (error "featurep! couldn't detect what module its in! (in %s)" path))
      (setq flag module
            module (car module-pair)
            submodule (cdr module-pair))))
  (if flag
      (and (memq flag (doom-module-get module submodule :flags)) t)
    (doom-module-p module submodule)))


;;
;; Module package macros
;;

(defmacro package! (name &rest plist)
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
   Do not update this package if FORM is non-nil."
  (declare (indent defun))
  (doom--assert-stage-p 'packages #'package!)
  (cond ((memq name doom-disabled-packages) nil)
        ((let ((disable (plist-get plist :disable)))
           (and disable (eval disable)))
         (push name doom-disabled-packages)
         (setq doom-packages (map-delete doom-packages name))
         nil)
        ((let* ((old-plist (assq name doom-packages))
                (pkg-recipe (or (plist-get plist :recipe)
                                (and old-plist (plist-get old-plist :recipe))))
                (pkg-pin    (or (plist-get plist :pin)
                                (and old-plist (plist-get old-plist :pin)))))
           (when pkg-recipe
             (when (= 0 (% (length pkg-recipe) 2))
               (plist-put plist :recipe (cons name pkg-recipe)))
             (when pkg-pin
               (plist-put plist :pin nil)))
           (dolist (prop '(:ignore :freeze))
             (let ((val (plist-get plist prop)))
               (when val
                 (plist-put plist prop (eval val)))))
           `(progn
              ,(when (and pkg-pin t)
                 `(map-put package-pinned-packages ',name ,pkg-pin))
              (map-put doom-packages ',name ',plist))))))

(defmacro packages! (&rest packages)
  "A convenience macro like `package!', but allows you to declare multiple
packages at once.

Only use this macro in a module's packages.el file."
  (doom--assert-stage-p 'packages #'packages!)
  `(progn ,@(cl-loop for desc in packages collect `(package! ,@desc))))

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
     (load! packages ,(doom-module-find-path module submodule) t)))


;;
;; Commands
;;

(defun doom//reload ()
  "Reload your private Doom config. Experimental!"
  (interactive)
  (doom//reload-load-path)
  (load (concat doom-private-dir "init.el") nil nil 'nosuffix)
  (let ((doom--stage 'config))
    (load (concat doom-private-dir "config.el") nil nil 'nosuffix))
  (message "Private config reloaded"))

(defun doom-packages--read-if-cookies (file)
  "Returns the value of the ;;;###if predicate form in FILE."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (if (and (re-search-forward "^;;;###if " nil t)
             (<= (line-number-at-pos) 3))
        (let ((load-file-name file))
          (eval (sexp-at-point)))
      t)))

(defun doom-packages--async-run (fn)
  (let* ((default-directory doom-emacs-dir))
    (compile (format "%s --quick --batch -l core/core.el -f %s"
                     (executable-find "emacs")
                     (symbol-name fn)))
    (while compilation-in-progress
      (sit-for 1))))

(defun doom-packages--files (dir pattern)
  "Like `directory-files-recursively', but traverses symlinks."
  (cl-letf (((symbol-function #'file-symlink-p) #'ignore))
    (directory-files-recursively dir pattern)))

(defun doom//reload-load-path ()
  "Reload `load-path' and recompile files (if necessary).

Use this when `load-path' is out of sync with your plugins. This should only
happen if you manually modify/update/install packages from outside Emacs, while
an Emacs session is running.

This isn't necessary if you use Doom's package management commands because they
call `doom//reload-load-path' remotely (through emacsclient)."
  (interactive)
  (when (file-exists-p doom-packages-file)
    (delete-file doom-packages-file))
  (cond ((and noninteractive (not (daemonp)))
         (require 'server)
         (when (server-running-p)
           (message "Reloading active Emacs session...")
           (server-eval-at server-name '(doom//reload-load-path))))
        (t
         (doom-initialize t)
         (message "%d packages reloaded" (length package-alist))
         (run-hooks 'doom-reload-hook))))

(defvar generated-autoload-load-name)
(defun doom//reload-autoloads ()
  "Refreshes the autoloads.el file, specified by `doom-autoload-file'.

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates an autoloads file at the path specified
by `doom-autoload-file'. This file tells Emacs where to find lazy-loaded
functions.

This should be run whenever init.el or an autoload file is modified. Running
'make autoloads' from the commandline executes this command."
  (interactive)
  ;; This function must not use autoloaded functions or external dependencies.
  ;; It must assume nothing is set up!
  (if (not noninteractive)
      ;; This is done in another instance to protect the current session's state
      ;; in case this function has side effects.
      (progn
        (doom-packages--async-run 'doom//reload-autoloads)
        (load doom-autoload-file t nil t))
    (let ((default-directory doom-emacs-dir)
          (targets
           (file-expand-wildcards
            (expand-file-name "autoload/*.el" doom-core-dir))))
      (dolist (path (doom-module-load-path))
        (let ((auto-dir  (expand-file-name "autoload" path))
              (auto-file (expand-file-name "autoload.el" path)))
          (when (file-exists-p auto-file)
            (push auto-file targets))
          (when (file-directory-p auto-dir)
            (dolist (file (doom-packages--files auto-dir "\\.el$"))
              (push file targets)))))
      (when (file-exists-p doom-autoload-file)
        (delete-file doom-autoload-file)
        (message "Deleted old autoloads.el"))
      (message "Generating new autoloads.el")
      (dolist (file (mapcar #'file-truename (reverse targets)))
        (let ((generated-autoload-load-name file))
          (message
           (cond ((not (doom-packages--read-if-cookies file))
                  "⚠ Ignoring %s")
                 ((update-file-autoloads file nil doom-autoload-file)
                  "✕ Nothing in %s")
                 ("✓ Scanned %s"))
           (if (file-in-directory-p file default-directory)
               (file-relative-name file)
             (abbreviate-file-name file)))))
      (make-directory (file-name-directory doom-autoload-file) t)
      (let ((buf (find-file-noselect doom-autoload-file t))
            (load-path (append (list doom-emacs-dir)
                               doom-psuedo-module-dirs
                               doom-modules-dirs
                               load-path))
            current-sexp)
        (unwind-protect
            (condition-case-unless-debug ex
                (with-current-buffer buf
                  (goto-char (point-min))
                  (while (re-search-forward "^\\s-*(" nil t)
                    (unless (or (nth 4 (syntax-ppss))
                                (nth 3 (syntax-ppss)))
                      ;; Replace autoload paths with absolute paths for faster
                      ;; resolution during load and simpler `load-path'
                      (when (memq (sexp-at-point) '(autoload custom-autoload))
                        (save-excursion
                          (forward-sexp 2)
                          (let ((pt (point)))
                            (forward-sexp 1)
                            (when-let* ((sexp (thing-at-point 'sexp t))
                                        (path (eval (read sexp) t)))
                              (when (and (stringp path) (not (file-name-absolute-p path)))
                                (delete-region pt (point))
                                (if-let* ((lib (locate-library path)))
                                    (insert " \"" (file-name-sans-extension lib) "\"")
                                  (warn "Couldn't find absolute path for: %s" path)))))))
                      ;; Run each form in autoloads to see if there are any
                      ;; errors. We do it piecemeal because that will tell us
                      ;; more about where the issue originated.
                      (save-excursion
                        (backward-char)
                        (setq current-sexp (read (thing-at-point 'sexp t)))
                        (eval current-sexp t))
                      (forward-char)))
                  (save-buffer)
                  (message "Done!"))
              ('error
               (delete-file doom-autoload-file)
               (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                      (nth 0 current-sexp)
                      (nth 1 current-sexp)
                      (car ex) (error-message-string ex))))
          (kill-buffer buf))))))

(defun doom//byte-compile (&optional modules recompile-p)
  "Byte compiles your emacs configuration.

init.el is always byte-compiled by this.

If MODULES is specified (a list of module strings, e.g. \"lang/php\"), those are
byte-compiled. Otherwise, all enabled modules are byte-compiled, including Doom
core. It always ignores unit tests and files with `no-byte-compile' enabled.

Doom was designed to benefit from byte-compilation, but the process may take a
while. Also, while your config files are byte-compiled, changes to them will not
take effect! Use `doom//clean-byte-compiled-files' or `make clean' to remove
these files.

If RECOMPILE-P is non-nil, only recompile out-of-date files."
  (interactive
   (list nil current-prefix-arg))
  (let ((default-directory doom-emacs-dir)
        (recompile-p (or recompile-p (and (member "-r" (cdr argv)) t)))
        (argv (delete "-r" argv)))
    (if (not noninteractive)
        ;; This is done in another instance to protect the current session's
        ;; state, because this function has side effects.
        (doom-packages--async-run 'doom//byte-compile)
      (let ((total-ok   0)
            (total-fail 0)
            (total-noop 0)
            (modules (or modules (cdr argv)))
            compile-targets)
        ;; Ensure that Doom has been fully loaded, some of its state may be
        ;; pertinent to files compiled later.
        (let (noninteractive)
          ;; Core libraries aren't fully loaded in a noninteractive session, so
          ;; we pretend to be interactive and reinitialize
          (doom-initialize)
          ;; In case autoloads.el hasn't been properly generated at this point.
          (unless (file-exists-p doom-autoload-file)
            (mapc #'load (file-expand-wildcards (expand-file-name "autoload/*.el" doom-core-dir)))))
        ;; Assemble el files we want to compile; taking into account that
        ;; MODULES may be a list of MODULE/SUBMODULE strings from the command
        ;; line.
        (setq compile-targets
              (cl-loop for target
                       in (or modules (append (list doom-core-dir) (doom-module-load-path)))
                       if (equal target "core")
                        nconc (nreverse (doom-packages--files doom-core-dir "\\.el$"))
                        and collect (expand-file-name "init.el" doom-private-dir)
                       else if (file-directory-p target)
                        nconc (nreverse (doom-packages--files target "\\.el$"))
                       else if (cl-member target doom-psuedo-module-dirs :test #'file-in-directory-p)
                        nconc (nreverse (doom-packages--files it "\\.el$"))
                       else if (string-match "^\\([^/]+\\)/\\([^/]+\\)$" target)
                        nconc (nreverse (doom-packages--files
                                         (doom-module-find-path
                                          (intern (format ":%s" (match-string 1 target)))
                                          (intern (match-string 2 target)))
                                         "\\.el$"))
                       else if (file-exists-p target)
                        collect target
                       finally do (setq argv nil)))
        (if (not compile-targets)
            (message "No targets to compile")
          (condition-case ex
              (let ((use-package-expand-minimally t))
                (push (expand-file-name "init.el" doom-emacs-dir) compile-targets)
                (dolist (target (cl-delete-duplicates (mapcar #'file-truename compile-targets) :test #'string=))
                  (when (or (not recompile-p)
                            (let ((elc-file (byte-compile-dest-file target)))
                              (and (file-exists-p elc-file)
                                   (file-newer-than-file-p target elc-file))))
                    (let ((result (if (doom-packages--read-if-cookies target)
                                      (byte-compile-file target)
                                    'no-byte-compile))
                          (short-name (if (file-in-directory-p target doom-emacs-dir)
                                          (file-relative-name target doom-emacs-dir)
                                        (abbreviate-file-name target))))
                      (cl-incf
                       (cond ((eq result 'no-byte-compile)
                              (message! (dark (white "⚠ Ignored %s" short-name)))
                              total-noop)
                             ((null result)
                              (message! (red "✕ Failed to compile %s" short-name))
                              total-fail)
                             (t
                              (message! (green "✓ Compiled %s" short-name))
                              (quiet! (load target t t))
                              total-ok))))))
                (message!
                 (bold
                  (color (if (= total-fail 0) 'green 'red)
                         "%s %s file(s) %s"
                         (if recompile-p "Recompiled" "Compiled")
                         (format "%d/%d" total-ok (- (length compile-targets) total-noop))
                         (format "(%s ignored)" total-noop)))))
            (error
             (message! (red "\n%%s\n\n%%s\n\n%%s")
                       "There were breaking errors."
                       (error-message-string ex)
                       "Reverting changes...")
             (quiet! (doom//clean-byte-compiled-files))
             (message! (green "Finished (nothing was byte-compiled)")))))))))

(defun doom//byte-compile-core (&optional recompile-p)
  "Byte compile the core Doom files.

This is faster than `doom//byte-compile', still yields considerable performance
benefits, and is more reliable in an ever-changing Emacs config (since you won't
likely change core files directly).

If RECOMPILE-P is non-nil, only recompile out-of-date core files."
  (interactive "P")
  (if (not noninteractive)
      ;; This is done in another instance to protect the current session's
      ;; state. `doom-initialize-packages' will have side effects otherwise.
      (doom-packages--async-run 'doom//byte-compile-core)
    (doom//byte-compile (list "core") recompile-p)))

(defun doom//byte-recompile-plugins ()
  "Recompile all installed plugins. If you're getting odd errors after upgrading
(or downgrading) Emacs, this may fix it."
  (interactive)
  (if (not noninteractive)
      ;; This is done in another instance to protect the current session's
      ;; state. `doom-initialize-packages' will have side effects otherwise.
      (doom-packages--async-run 'doom//byte-recompile-plugins)
    (byte-recompile-directory package-user-dir 0 t)))

(defun doom//clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration. This excludes
compiled packages.'"
  (interactive)
  (unless
      (cl-loop with default-directory = doom-emacs-dir
               for path
               in (append (file-expand-wildcards "*.elc" t)
                          (doom-packages--files doom-core-dir "\\.elc$")
                          (cl-loop for dir in (doom-module-load-path)
                                   nconc (doom-packages--files dir "\\.elc$")))
               for truepath = (file-truename path)
               if (file-exists-p truepath)
               collect path
               and do (delete-file truepath)
               and do
               (message "✓ Deleted %s"
                        (if (file-in-directory-p truepath default-directory)
                            (file-relative-name truepath)
                          (abbreviate-file-name path))))
    (message "Everything is clean")))


;;
;; Make package.el cooperate with Doom
;;

(defun doom*initialize-packages (&rest _) (package-initialize))

(advice-add #'package-delete           :before #'doom*initialize-packages)
(advice-add #'package-install          :before #'doom*initialize-packages)
(advice-add #'package-refresh-contents :before #'doom*initialize-packages)
(advice-add #'package-reinstall        :before #'doom*initialize-packages)

;; Updates QUELPA after deleting a package
(advice-add #'package-delete :after #'doom*package-delete)

;; Replace with Doom variants
(advice-add #'package-autoremove :override #'doom//packages-autoremove)
(advice-add #'package-install-selected-packages :override #'doom//packages-install)


;;
;; Cross-module configuration
;;

;; I needed a way to reliably cross-configure modules without worrying about
;; whether they were enabled or not, so I wrote `set!'. If a setting doesn't
;; exist at runtime, the `set!' call is ignored and its arguments are left
;; unevaluated (and entirely omitted when byte-compiled).
(defvar doom-settings nil)

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting. Like `defmacro', this should return a form to be executed
when called with `set!'. FORMS are not evaluated until `set!' calls it.

See `doom/describe-setting' for a list of available settings.

Do not use this for configuring Doom core."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  (let ((fn (intern (format "doom--set%s" keyword))))
    `(progn
       (defun ,fn ,arglist
         ,docstring
         ,@forms)
       (map-put doom-settings ,keyword #',fn))))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if doesn't exist. See
`doom/describe-setting' for a list of available settings.

VALUES doesn't get evaluated if the KEYWORD setting doesn't exist."
  (declare (indent defun))
  (unless values
    (error "Empty set! for %s" keyword))
  (if-let* ((fn (cdr (assq keyword doom-settings))))
      (apply fn values)
    (when doom-debug-mode
      (message "No setting found for %s" keyword)
      nil)))

(provide 'core-packages)
;;; core-packages.el ends here
