;;; core-packages.el --- package management system -*- lexical-binding: t; -*-

;; Emacs package management is opinionated, and so am I. I've bound together
;; `use-package', `quelpa' and package.el to create my own, rolling-release,
;; lazily-loaded package management system for Emacs.
;;
;; The three key commands are:
;;
;; + `bin/doom install` or `doom//packages-install': Installs packages that are
;;   wanted, but not installed.
;; + `bin/doom update` or `doom//packages-update': Updates packages that are
;;   out-of-date.
;; + `bin/doom autoremove` or `doom//packages-autoremove': Uninstalls packages
;;   that are no longer needed.
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
  "Non-nil if `doom-initialize' has run.")

(defvar doom-init-modules-p nil
  "Non-nil if `doom-initialize-modules' has run.")

(defvar doom-init-time nil
  "The time it took, in seconds, for DOOM Emacs to initialize.")

(defvar doom-modules ()
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-modules-dirs
  (list (expand-file-name "modules/" doom-private-dir) doom-modules-dir)
  "A list of module root directories. Order determines priority.")

(defvar doom-psuedo-module-dirs (list doom-private-dir)
  "Additional paths for modules that are outside of `doom-modules-dirs'.
`doom//reload-doom-autoloads', `doom//byte-compile' and
`doom-initialize-packages' will include the directories in this list.")

(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

(defvar doom-core-packages '(persistent-soft use-package quelpa async)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defvar doom-site-load-path load-path
  "The starting load-path, before it is altered by `doom-initialize'.")

(defvar doom-autoload-file (concat doom-local-dir "autoloads.el")
  "Where `doom//reload-doom-autoloads' will generate its core autoloads file.")

(defvar doom-package-autoload-file (concat doom-local-dir "autoloads.pkg.el")
  "Where `doom//reload-package-autoloads' will generate its package.el autoloads
file.")

(defvar doom-reload-hook nil
  "A list of hooks to run when `doom//reload-load-path' is called.")

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
           (if doom-modules (hash-table-count doom-modules) 0)
           (or doom-init-time
               (setq doom-init-time (float-time (time-subtract (current-time) before-init-time))))))

(defun doom|post-init ()
  "Run `doom-post-init-hook'. That's all."
  (run-hooks 'doom-post-init-hook))

(defun doom|run-all-startup-hooks ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:

  emacs -Q -l init.el -f doom|run-all-startup-hooks"
  (run-hooks 'after-init-hook 'delayed-warnings-hook
             'emacs-startup-hook 'term-setup-hook
             'window-setup-hook))


;;
;; Bootstrap helpers
;;

(defun doom-ensure-packages-initialized (&optional force-p)
  "Make sure package.el is initialized."
  (when (or force-p (not package--initialized))
    (require 'package)
    (setq package-activated-list nil
          package--initialized nil)
    (let (byte-compile-warnings)
      (condition-case _
          (quiet! (package-initialize))
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

(defun doom-ensure-core-directories ()
  "Make sure all Doom's essential local directories (in and including
`doom-local-dir') exist."
  (dolist (dir (list doom-local-dir doom-etc-dir doom-cache-dir doom-packages-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))


;;
;; Bootstrap API
;;

(autoload 'doom//reload-doom-autoloads "autoload/modules" nil t)
(autoload 'doom//reload-package-autoloads "autoload/modules" nil t)

(defun doom-initialize (&optional force-p)
  "Bootstrap Doom, if it hasn't already (or if FORCE-P is non-nil).

The bootstrap process involves making sure 1) the essential directories exist,
2) the core packages are installed, 3) `doom-autoload-file' and
`doom-package-autoload-file' exist and have been loaded, and 4) Doom's core
files are loaded.

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

The overall load order of Doom is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  `doom-pre-init-hook'
  ~/.doom.d/init.el
  Module init.el files
  `doom-init-hook'
  Module config.el files
  ~/.doom.d/config.el
  `after-init-hook'
  `emacs-startup-hook'
  `doom-post-init-hook' (at end of `emacs-startup-hook')

Module load order is determined by your `doom!' block. See `doom-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (when (or force-p (not doom-init-p))
    ;; Set this to prevent infinite recursive calls to `doom-initialize'
    (setq doom-init-p t)
    ;; `doom-autoload-file' tells Emacs where to load all its autoloaded
    ;; functions from. This includes everything in core/autoload/*.el and all
    ;; the autoload files in your enabled modules.
    (unless (doom-initialize-autoloads doom-autoload-file force-p)
      (doom-ensure-core-directories)
      (doom-ensure-packages-initialized force-p)
      (doom-ensure-core-packages)
      ;; Regenerate `doom-autoload-file', which tells Doom where to find all its
      ;; module autoloaded functions.
      (unless (or force-p noninteractive)
        (doom//reload-doom-autoloads)))
    ;; Loads `doom-package-autoload-file', which caches `load-path',
    ;; `auto-mode-alist', `Info-directory-list', `doom-disabled-packages' and
    ;; `package-activated-list'. A big reduction in startup time.
    (unless (doom-initialize-autoloads doom-package-autoload-file force-p)
      (unless (or force-p noninteractive)
        (doom//reload-package-autoloads))))
  ;; Initialize Doom core
  (require 'core-os)
  (unless noninteractive
    (add-hook! 'emacs-startup-hook
      #'(doom|post-init doom|display-benchmark))
    (require 'core-ui)
    (require 'core-editor)
    (require 'core-projects)
    (require 'core-keybinds)))

(defun doom-initialize-modules (&optional force-p)
  "Loads the init.el in `doom-private-dir' and sets up hooks for a healthy
session of Dooming. Will noop if used more than once, unless FORCE-P is
non-nil."
  (when (or force-p (not doom-init-modules-p))
    ;; Set `doom-init-modules-p' early, so `doom-pre-init-hook' won't infinitely
    ;; recurse by accident if any of them need `doom-initialize-modules'.
    (setq doom-init-modules-p t)
    (when doom-private-dir
      (let ((load-prefer-newer t))
        (load (expand-file-name "init" doom-private-dir)
              'noerror 'nomessage)))))

(defun doom-initialize-autoloads (file &optional clear-p)
  "Tries to load FILE (an autoloads file). Otherwise tries to regenerate it. If
CLEAR-P is non-nil, regenerate it anyway."
  (unless clear-p
    (load (file-name-sans-extension file) 'noerror 'nomessage)))

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
      ;; `doom-packages'
      (unless (eq force-p 'internal)
        ;; `package-alist'
        (when (or force-p (not (bound-and-true-p package-alist)))
          (setq load-path doom-site-load-path)
          (doom-ensure-packages-initialized t))

        ;; `quelpa-cache'
        (when (or force-p (not (bound-and-true-p quelpa-cache)))
          (require 'quelpa)
          (setq quelpa-initialized-p nil)
          (or (quelpa-setup-p)
              (error "Could not initialize quelpa"))))

      (when (or force-p (not doom-packages))
        (let ((doom-modules (doom-module-table)))
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
                       for path = (doom-module-path (car key) (cdr key) "packages.el")
                       do (let ((doom--current-module key)) (_load path t)))
              (cl-loop for dir in doom-psuedo-module-dirs
                       do (_load (expand-file-name "packages.el" dir) t)))))))))


;;
;; Module API
;;

(defun doom-module-p (category module)
  "Returns t if CATEGORY MODULE is enabled (ie. present in `doom-modules')."
  (and (hash-table-p doom-modules)
       (gethash (cons category module) doom-modules)
       t))

(defun doom-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY, specifically, if set."
  (when-let* ((plist (gethash (cons category module) doom-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun doom-module-put (category module property value &rest rest)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be additional pairs
of PROPERTY and VALUEs."
  (when-let* ((plist (doom-module-get category module)))
    (plist-put plist property value)
    (when rest
      (when (cl-oddp (length rest))
        (signal 'wrong-number-of-arguments (list (length rest))))
      (while rest
        (plist-put rest (pop rest) (pop rest))))
    (puthash (cons category module) plist doom-modules)))

(defun doom-module-set (category module &rest plist)
  "Enables a module by adding it to `doom-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled category flags
  :path  [STRING]       path to category root directory

Example:
  (doom-module-set :lang 'haskell :flags '(+intero))"
  (when plist
    (let ((old-plist (doom-module-get category module)))
      (unless (plist-member plist :flags)
        (plist-put plist :flags (plist-get old-plist :flags)))
      (unless (plist-member plist :path)
        (plist-put plist :path (or (plist-get old-plist :path)
                                   (doom-module-locate-path category module))))))
  (let ((key (cons category module)))
    (puthash key plist doom-modules)))

(defun doom-module-path (category module &optional file)
  "Like `expand-file-name', but expands FILE relative to CATEGORY (keywordp) and
MODULE (symbol).

If the category isn't enabled this will always return nil. For finding disabled
modules use `doom-module-locate-path'."
  (let ((path (doom-module-get category module :path)))
    (if file (expand-file-name file path)
      path)))

(defun doom-module-locate-path (category &optional module file)
  "Searches `doom-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path.

This doesn't require modules to be enabled. For enabled modules us
`doom-module-path'."
  (when (keywordp category)
    (setq category (substring (symbol-name category) 1)))
  (when (and module (symbolp module))
    (setq module (symbol-name module)))
  (cl-loop for default-directory in doom-modules-dirs
           for path = (concat category "/" module "/" file)
           if (file-exists-p path)
           return (expand-file-name path)))

(defun doom-module-from-path (&optional path)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path)."
  (or doom--current-module
      (when path
        (save-match-data
          (setq path (file-truename path))
          (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)/.*$" path)
            (when-let* ((module (match-string 1 path))
                        (submodule (match-string 2 path)))
              (cons (intern (concat ":" module))
                    (intern submodule))))))))

(defun doom-module-load-path ()
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added, if the file exists."
  (append (cl-loop for plist being the hash-values of doom-modules
                   collect (plist-get plist :path))
          (cl-remove-if-not #'file-directory-p doom-psuedo-module-dirs)))

(defun doom-module-table (&optional modules)
  "Converts MODULES (a malformed plist) into a hash table of modules, fit for
`doom-modules'. If MODULES is omitted, it will fetch your module mplist from the
`doom!' block in your private init.el file."
  (let* ((doom-modules (make-hash-table :test #'equal
                                        :size (if modules (length modules) 100)
                                        :rehash-threshold 1.0)))
    (when (null modules)
      (let* ((init-file (expand-file-name "init.el" doom-private-dir))
             (short-init-file (abbreviate-file-name init-file)))
        (if (not (file-exists-p init-file))
            (error "%s doesn't exist" short-init-file)
          (with-temp-buffer
            (delay-mode-hooks (emacs-lisp-mode))
            (insert-file-contents-literally init-file)
            (when (re-search-forward "^\\s-*\\((doom! \\)" nil t)
              (goto-char (match-beginning 1))
              (setq modules (cdr (sexp-at-point))))))
        (unless (or modules noninteractive)
          (warn "Couldn't gather module list from %s" short-init-file))))
    (if (eq modules t) (setq modules nil))
    (let (category)
      (dolist (m modules)
        (cond ((keywordp m) (setq category m))
              ((not category) (error "No module category specified for %s" m))
              ((let ((module (if (listp m) (car m) m))
                     (flags  (if (listp m) (cdr m))))
                 (if-let* ((path (doom-module-locate-path category module)))
                     (doom-module-set category module :flags flags :path path)
                   (message "Couldn't find the %s %s module" category module)))))))
    doom-modules))


;;
;; Use-package modifications
;;

(autoload 'use-package "use-package-core" nil nil t)

;; Adds the :after-call custom keyword to `use-package' (and consequently,
;; `def-package!'). :after-call takes a symbol ro list of symbols. These symbols
;; can be functions to hook variables.
;;
;;   (use-package X :after-call find-file-hook)
;;
;; This will load X on the first invokation of `find-file-hook' (then it will
;; remove itself from the hook).
(defvar doom--deferred-packages-alist ())
(after! use-package-core
  (add-to-list 'use-package-deferring-keywords :after-call nil #'eq)

  (setq use-package-keywords
        (use-package-list-insert :after-call use-package-keywords :after))

  (defalias 'use-package-normalize/:after-call
    'use-package-normalize-symlist)

  (defun use-package-handler/:after-call (name-symbol _keyword hooks rest state)
    (let ((fn (intern (format "doom|transient-hook--load-%s" name-symbol)))
          (hooks (delete-dups hooks)))
      (if (plist-get state :demand)
          (use-package-process-keywords name rest state)
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (require ',name-symbol)
                   (dolist (hook (cdr (assq ',name-symbol doom--deferred-packages-alist)))
                     (if (functionp hook)
                         (advice-remove hook #',fn)
                       (remove-hook hook #',fn)))
                   (map-delete doom--deferred-packages-alist ',name-symbol)
                   (fmakunbound ',fn))))
         (cl-mapcan (lambda (hook)
                      (if (functionp hook)
                          `((advice-add #',hook :before #',fn))
                        `((add-hook ',hook #',fn))))
                    hooks)
         `((map-put doom--deferred-packages-alist
                    ',name-symbol
                    '(,@hooks ,@(cdr (assq name-symbol doom--deferred-packages-alist)))))
         (use-package-process-keywords name rest state))))))


;;
;; Module config macros
;;

(defmacro doom! (&rest modules)
  "Bootstraps DOOM Emacs and its modules.

The bootstrap process involves making sure the essential directories exist, core
packages are installed, `doom-autoload-file' is loaded, `doom-packages-file'
cache exists (and is loaded) and, finally, loads your private init.el (which
should contain your `doom!' block).

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

The overall load order of Doom is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  `doom-pre-init-hook'
  ~/.doom.d/init.el
  Module init.el files
  `doom-init-hook'
  Module config.el files
  ~/.doom.d/config.el
  `after-init-hook'
  `emacs-startup-hook'
  `doom-post-init-hook' (at end of `emacs-startup-hook')

Module load order is determined by your `doom!' block. See `doom-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (let ((doom-modules (doom-module-table (or modules t)))
        init-forms config-forms)
    (maphash (lambda (key plist)
               (let ((path (plist-get plist :path)))
                 (push `(let ((doom--current-module ',key)) (load! "init" ,path t))
                       init-forms)
                 (push `(let ((doom--current-module ',key)) (load! "config" ,path t))
                       config-forms)))
             doom-modules)
    `(let (file-name-handler-alist)
       (setq doom-modules ',doom-modules)
       ,@(nreverse init-forms)
       (run-hooks 'doom-init-hook)
       (unless noninteractive
         (let ((doom--stage 'config))
           ,@(nreverse config-forms)
           (when doom-private-dir
             (let ((load-prefer-newer t))
               (load ,(expand-file-name "config" doom-private-dir)
                     t (not doom-debug-mode)))))))))

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
    `(use-package ,name ,@plist)))

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
        ((error "'%s' isn't a valid hook for def-package-hook!" when))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (unless path
    (setq path (or (and (bound-and-true-p byte-compile-current-file)
                        (file-name-directory byte-compile-current-file))
                   (and load-file-name (file-name-directory load-file-name))
                   (and buffer-file-name
                        (file-name-directory buffer-file-name))
                   (error "Could not detect path to look for '%s' in" filename))))
  `(load ,(if path
              `(expand-file-name ,filename ,path)
            filename)
         ,noerror ,(not doom-debug-mode)))

(defmacro require! (module submodule &optional reload-p &rest plist)
  "Loads the module specified by MODULE (a property) and SUBMODULE (a symbol).

The module is only loaded once. If RELOAD-P is non-nil, load it again."
  (let ((enabled-p (doom-module-p module submodule)))
    (when (or (not enabled-p) plist)
      (apply #'doom-module-set module submodule
             (mapcar #'eval plist)))
    (when (or reload-p (not enabled-p))
      (let ((module-path (doom-module-locate-path module submodule)))
        (if (file-directory-p module-path)
            `(condition-case-unless-debug ex
                 (let ((doom--current-module ',(cons module submodule)))
                   (load! "init" ,module-path :noerror)
                   (let ((doom--stage 'config))
                     (load! "config" ,module-path :noerror)))
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
    (let* ((path (or (bound-and-true-p byte-compile-current-file)
                     load-file-name))
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
   Do not update this package if FORM is non-nil.

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
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
              (map-put doom-packages ',name ',plist)
              t)))))

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


;;
;; Make package.el cooperate with Doom
;;

;; Updates QUELPA after deleting a package
(advice-add #'package-delete :after #'doom*package-delete)

;; Replace with Doom variants
(advice-add #'package-autoremove :override #'doom//packages-autoremove)
(advice-add #'package-install-selected-packages :override #'doom//packages-install)


;;
;; Cross-module configuration
;;

;; I needed a way to reliably cross-configure modules without littering my
;; modules with `after!' blocks or testing whether they were enabled, so I wrote
;; `set!'. If a setting doesn't exist at runtime, the `set!' call is ignored and
;; its arguments are left unevaluated (and entirely omitted when byte-compiled).

(defvar doom-settings nil
  "An alist mapping setting keywords to functions.")

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
