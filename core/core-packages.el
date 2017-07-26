;;; core-packages.el --- package management system -*- lexical-binding: t; -*-

;; Emacs package management is opinionated. Unfortunately, so am I. I've bound
;; together `use-package', `quelpa' and package.el to create my own,
;; rolling-release, lazily-loaded package management system for Emacs.
;;
;; The three key commands are:
;;
;; + `make install` or `doom/packages-install': Installs packages that are
;;   wanted, but not installed.
;; + `make update` or `doom/packages-update': Updates packages that are
;;   out-of-date.
;; + `make autoremove` or `doom/packages-autoremove': Uninstalls packages that
;;   are no longer needed.
;;
;; This system reads packages.el files located in each activated module (and one
;; in `doom-core-dir'). These contain `package!` blocks that tell DOOM what
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
;; You should be able to use package.el commands without any conflicts, but to
;; be absolutely certain use the doom alternatives:
;;
;;    + `package-install':          `doom/install-package'
;;    + `package-reinstall':        `doom/reinstall-package'
;;    + `package-delete':           `doom/delete-package'
;;    + `package-update':           `doom/update-package'
;;    + `package-autoremove':       `doom/packages-autoremove'
;;    + `package-refresh-contents': `doom/refresh-packages'
;;
;; See core/autoload/packages.el for more functions.

(defvar doom-init-p nil
  "Non-nil if doom is done initializing (once `doom-post-init-hook' is done). If
this is nil after Emacs has started something is wrong.")

(defvar doom-package-init-p nil
  "If non-nil, doom's package system has been initialized (by
`doom-initialize'). This will be nill if you byte-compile your configuration (as
intended).")

(defvar doom-init-time nil
  "The time it took, in seconds, for DOOM Emacs to initialize.")

(defvar doom-modules ()
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

(defvar doom-core-packages
  '(persistent-soft quelpa use-package)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defvar doom-reload-hook nil
  "A list of hooks to run when `doom/reload' is called.")

(defvar doom--site-load-path load-path
  "The load path of built in Emacs libraries.")

(defvar doom--package-load-path ()
  "The load path of package libraries installed via ELPA or QUELPA.")

(defvar doom--base-load-path
  (append (list doom-core-dir doom-modules-dir)
          doom--site-load-path)
  "A backup of `load-path' before it was altered by `doom-initialize'. Used as a
base by `doom!' and for calculating how many packages exist.")

(defvar doom--refresh-p nil)

(setq load-prefer-newer (or noninteractive doom-debug-mode)
      package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" doom-packages-dir)
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.

      ;; security settings
      gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

      use-package-always-defer t
      use-package-always-ensure nil
      use-package-debug nil
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


;;
;; Bootstrap function
;;

(defun doom-initialize (&optional force-p)
  "Initialize installed packages (using package.el) and ensure the core packages
are installed. If you byte-compile core/core.el, this function will be avoided
to speed up startup."
  ;; Called early during initialization; only use native functions!
  (when (or (not doom-package-init-p) force-p)
    (unless noninteractive
      (message "Doom initialized"))

    (setq load-path doom--base-load-path
          package-activated-list nil)

    ;; Ensure core folders exist
    (dolist (dir (list doom-local-dir doom-etc-dir doom-cache-dir package-user-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))

    (package-initialize t)
    ;; Sure, we could let `package-initialize' fill `load-path', but package
    ;; activation costs precious milliseconds and does other stuff I don't
    ;; really care about (like load autoload files). My premature optimization
    ;; quota isn't filled yet.
    ;;
    ;; Also, in some edge cases involving package initialization during a
    ;; non-interactive session, `package-initialize' fails to fill `load-path'.
    ;; If we want something done right, do it ourselves!
    (setq doom--package-load-path (directory-files package-user-dir t "^[^.]" t)
          load-path (append load-path doom--package-load-path))

    ;; Ensure core packages are installed
    (dolist (pkg doom-core-packages)
      (unless (package-installed-p pkg)
        (unless doom--refresh-p
          (package-refresh-contents)
          (setq doom--refresh-p t))
        (let ((inhibit-message t))
          (package-install pkg))
        (if (package-installed-p pkg)
            (message "Installed %s" pkg)
          (error "Couldn't install %s" pkg))))

    (load "quelpa" nil t)
    (load "use-package" nil t)

    (setq doom-package-init-p t)))

(defun doom-initialize-autoloads ()
  "Ensures that `doom-autoload-file' exists and is loaded. Otherwise run
`doom/reload-autoloads' to generate it."
  (unless (file-exists-p doom-autoload-file)
    (quiet! (doom/reload-autoloads))))

(defun doom-initialize-packages (&optional force-p load-p)
  "Crawls across your emacs.d to fill `doom-modules' (from init.el) and
`doom-packages' (from packages.el files), if they aren't set already.

If FORCE-P is non-nil, do it even if they are.

This aggressively reloads core autoload files."
  (doom-initialize force-p)
  (let ((noninteractive t)
        (load-fn
         (lambda (file &optional noerror)
           (condition-case-unless-debug ex
               (load file noerror :nomessage :nosuffix)
             ('error
              (error (format "(doom-initialize-packages) %s in %s: %s"
                             (car ex)
                             (file-relative-name file doom-emacs-dir)
                             (error-message-string ex))
                     :error))))))
    (when (or force-p (not doom-modules))
      (setq doom-modules nil)
      (funcall load-fn (expand-file-name "init.el" doom-emacs-dir))
      (funcall load-fn (doom-module-path :private user-login-name "init.el") t)
      (when load-p
        (cl-loop for file
                 in (append (nreverse (file-expand-wildcards (expand-file-name "core*.el" doom-core-dir)))
                            (file-expand-wildcards (expand-file-name "autoload/*.el" doom-core-dir))
                            (doom--module-paths "config.el"))
                 do (funcall load-fn file t)))
      (doom|finalize))
    (when (or force-p (not doom-packages))
      (setq doom-packages nil)
      (funcall load-fn (expand-file-name "packages.el" doom-core-dir))
      (dolist (file (doom--module-paths "packages.el"))
        (funcall load-fn file t)))))

(defun doom-initialize-modules (modules)
  "Adds MODULES to `doom-modules'. MODULES must be in mplist format.

  e.g '(:feature evil :lang emacs-lisp javascript java)"
  (unless doom-modules
    (setq doom-modules (make-hash-table :test #'equal :size (+ 5 (length modules)))))
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m)
             (setq mode m))
            ((not mode)
             (error "No namespace specified on `doom!' for %s" m))
            ((eq m '*)
             (doom-initialize-modules
              (cl-loop with modpath = (expand-file-name (substring (symbol-name mode) 1) doom-modules-dir)
                       for path in (directory-files modpath t "^\\w")
                       if (file-directory-p path)
                        collect (intern (file-name-nondirectory path)) into paths
                       finally return (cons mode paths))))
            (t
             (doom--enable-module mode m))))))

(defun doom-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (when (keywordp module)
    (setq module (substring (symbol-name module) 1)))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (expand-file-name (concat module "/" submodule "/" file)
                    doom-modules-dir))

(defun doom-module-loaded-p (module submodule)
  "Returns t if MODULE->SUBMODULE is present in `doom-modules'."
  (and doom-modules
       (gethash (cons module submodule) doom-modules)))

(defun doom--module-pairs ()
  "Returns `doom-modules' as a list of (MODULE . SUBMODULE) cons cells. The list
is sorted by order of insertion unless ALL-P is non-nil. If ALL-P is non-nil,
include all modules, enabled or otherwise."
  (if (hash-table-p doom-modules)
      (cl-loop for key being the hash-keys of doom-modules
               collect (cons (car key) (cdr key)))
    (error "doom-modules is uninitialized")))

(defun doom--module-paths (&optional append-file)
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added, if the file exists."
  (let (paths)
    (dolist (pair (doom--module-pairs) (nreverse paths))
      (let ((path (doom-module-path (car pair) (cdr pair) append-file)))
        (when (file-exists-p path)
          (push path paths))))))

(defun doom--enable-module (module submodule &optional force-p)
  "Adds MODULE and SUBMODULE to `doom-modules', if it isn't already there (or if
FORCE-P is non-nil). MODULE is a keyword, SUBMODULE is a symbol. e.g. :lang
'emacs-lisp.

Used by `require!' and `depends-on!'."
  (unless (or force-p (doom-module-loaded-p module submodule))
    (puthash (cons module submodule) t doom-modules)))

(defun doom--display-benchmark ()
  (message "Loaded %s packages in %.03fs"
           ;; Certainly imprecise, especially where custom additions to
           ;; load-path are concerned, but I don't mind a [small] margin of
           ;; error in the plugin count in exchange for faster startup.
           (- (length load-path) (length doom--base-load-path))
           (setq doom-init-time (float-time (time-subtract after-init-time before-init-time)))))


;;
;; Macros
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro doom! (&rest modules)
  "Bootstrap DOOM Emacs.

MODULES is an malformed plist of modules to load."
  (doom-initialize-modules modules)
  (when (and user-login-name
             (not (doom-module-loaded-p :private (intern user-login-name))))
    (doom--enable-module :private user-login-name))
  `(let (file-name-handler-alist)
     (setq doom-modules ',doom-modules)

     (unless noninteractive
       (require 'core-ui)         ; draw me like one of your French editors
       (require 'core-popups)     ; taming sudden yet inevitable windows
       (require 'core-editor)     ; baseline configuration for text editing
       (require 'core-projects)   ; making Emacs project-aware
       (require 'core-keybinds)   ; centralized keybind system + which-key

       (load ,(doom-module-path :private user-login-name "init") t t)
       ,@(cl-loop for (module . submodule) in (doom--module-pairs)
                  collect `(require! ,module ,submodule t))

       (when (display-graphic-p)
         (require 'server)
         (unless (server-running-p)
           (server-start)))

       (add-hook 'doom-init-hook #'doom--display-benchmark t))))

(defmacro def-package! (name &rest plist)
  "A thin wrapper around `use-package'.

Ignores the package if its NAME is present in `doom-disabled-packages'."
  (when (and (memq name doom-disabled-packages)
             (not (memq :disabled plist)))
    (setq plist (append (list :disabled t) plist)))
  `(use-package ,name ,@plist))

(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config :disable

If WHEN is :disable then BODY is ignored, and DOOM will be instructed to ignore
all `def-package!' blocks for PACKAGE.

WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Doom's config)."
  (declare (indent defun))
  (cond ((eq when :disable)
         (push package doom-disabled-packages)
         nil)
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
where to look for the file (a string representing a directory path), by default
it is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (let ((path (or (and path (or (and (symbolp path) (symbol-value path))
                                (and (stringp path) path)
                                (and (listp path) (eval path))))
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name (file-name-directory buffer-file-name))))
        (filename (cond ((stringp filesym) filesym)
                        ((symbolp filesym) (symbol-name filesym))
                        (t (error "load! expected a string or symbol, got %s (a %s)"
                                  filesym (type-of filesym))))))
    (unless path
      (error "Could not find %s" filename))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror ,(not doom-debug-mode))
        (unless noerror
          (error "Could not load! file %s" file))))))

(defmacro require! (module submodule &optional reload-p)
  "Loads the module specified by MODULE (a property) and SUBMODULE (a symbol).

The module is only loaded once. If RELOAD-P is non-nil, load it again."
  (let ((loaded-p (doom-module-loaded-p module submodule)))
    (when (or reload-p (not loaded-p))
      (unless loaded-p
        (doom--enable-module module submodule t))
      `(condition-case-unless-debug ex
           (load! config ,(doom-module-path module submodule) t)
         ('error
          (lwarn 'doom-modules :error
                 "%s in '%s %s' -> %s"
                 (car ex) ,module ',submodule (error-message-string ex)))))))

(defmacro featurep! (module submodule)
  "Convenience macro wrapper for `doom-module-loaded-p'."
  (doom-module-loaded-p module submodule))


;;
;; Declarative macros
;;

(defmacro package! (name &rest plist)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `doom-packages' with metadata about the packages Doom needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :recipe RECIPE        Takes a MELPA-style recipe (see `quelpa-recipe' in
                       `quelpa' for an example); for packages to be installed
                       from external sources.
 :pin ARCHIVE-NAME     Instructs ELPA to only look for this package in
                       ARCHIVE-NAME. e.g. \"org\". Ignored if RECIPE is present.
 :ignore FORM          Do not install this package if FORM is non-nil.
 :freeze FORM          Do not update this package if FORM is non-nil."
  (declare (indent defun))
  (let* ((old-plist (assq name doom-packages))
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
      (when-let (val (plist-get plist prop))
        (plist-put plist prop (eval val))))
    `(progn
       (when ,(and pkg-pin t)
         (cl-pushnew (cons ',name ,pkg-pin) package-pinned-packages
                     :test #'eq :key #'car))
       (when ,(and old-plist t)
         (assq-delete-all ',name doom-packages))
       (push ',(cons name plist) doom-packages))))

(defmacro depends-on! (module submodule)
  "Declares that this module depends on another.

Only use this macro in a module's packages.el file.

MODULE is a keyword, and SUBMODULE is a symbol. Under the hood, this simply
loads MODULE SUBMODULE's packages.el file."
  (doom--enable-module module submodule)
  `(load! packages ,(doom-module-path module submodule) t))


;;
;; Commands
;;

(defun doom/reload ()
  "Reload `load-path' and recompile files (if necessary).

Use this when `load-path' is out of sync with your plugins. This should only
happen if you manually modify/update/install packages from outside Emacs, while
an Emacs session is running.

This isn't necessary if you use Doom's package management commands because they
call `doom/reload' remotely (through emacsclient)."
  (interactive)
  (cond (noninteractive
         (message "Reloading...")
         (require 'server)
         (unless (ignore-errors (server-eval-at "server" '(doom/reload)))
           (message "Recompiling")
           (doom/recompile)))
        (t
         (doom-initialize t)
         (doom/recompile)
         (message "Reloaded %d packages" (length doom--package-load-path))
         (run-with-timer 1 nil #'redraw-display)
         (run-hooks 'doom-reload-hook))))

(defun doom/reload-autoloads ()
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
  (doom-initialize-packages (not noninteractive))
  (let ((evil-p (doom-module-loaded-p :feature 'evil))
        (targets
         (file-expand-wildcards
          (expand-file-name "autoload/*.el" doom-core-dir))))
    (dolist (path (doom--module-paths))
      (let ((auto-dir  (expand-file-name "autoload" path))
            (auto-file (expand-file-name "autoload.el" path)))
        (when (file-exists-p auto-file)
          (push auto-file targets))
        (when (file-directory-p auto-dir)
          (dolist (file (file-expand-wildcards (expand-file-name "*.el" auto-dir) t))
            ;; Make evil*.el autoload files a special case; don't load
            ;; them unless evil is enabled.
            (unless (and (string-prefix-p "evil" (file-name-nondirectory file))
                         (not evil-p))
              (push file targets))))))
    (when (file-exists-p doom-autoload-file)
      (delete-file doom-autoload-file)
      (message "Deleted old autoloads.el"))
    (dolist (file (reverse targets))
      (message (if (update-file-autoloads file nil doom-autoload-file)
                   "Nothing in %s"
                 "Scanned %s")
               (file-relative-name file doom-emacs-dir)))
    (let ((buf (get-file-buffer doom-autoload-file))
          current-sexp)
      (unwind-protect
          (condition-case-unless-debug ex
              (with-current-buffer buf
                (save-buffer)
                (goto-char (point-min))
                (while (re-search-forward "^(" nil t)
                  (save-excursion
                    (backward-char)
                    (setq current-sexp (read (thing-at-point 'sexp t)))
                    (eval current-sexp t))
                  (forward-char))
                (message "Finished generating autoloads.el!"))
            ('error
             (delete-file doom-autoload-file)
             (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                    (nth 0 current-sexp)
                    (nth 1 current-sexp)
                    (car ex) (error-message-string ex))))
        (kill-buffer buf)))))

(defun doom/compile (&optional lite-p only-recompile-p)
  "Byte compiles your emacs configuration.

Specifically, this byte-compiles init.el, core/*.el, core/autoload/*.el &
modules/*/*/**.el. It ignores unit tests and files with `no-byte-compile'
enabled.

DOOM Emacs was designed to benefit from byte-compilation, but the process may
take a while. Also, while your config files are byte-compiled, changes to them
will not take effect! Use `doom/clean-compiled' or `make clean' to undo
byte-compilation.

If LITE-P is non-nil, only compile the core DOOM files (init.el & core/**/*.el).

If ONLY-RECOMPILE-P is non-nil, only recompile out-of-date files."
  (interactive "P")
  ;; Ensure all relevant config files are loaded and up-to-date. This way we
  ;; don't need eval-when-compile and require blocks scattered all over.
  (doom-initialize-packages t noninteractive)
  (let ((targets
         (cond ((equal (car command-line-args-left) "--")
                (cl-loop for file in (cdr command-line-args-left)
                         if (file-exists-p file)
                         collect (expand-file-name file)
                         finally do (setq command-line-args-left nil)) )
               (t
                (append (list (expand-file-name "init.el" doom-emacs-dir)
                              doom-core-dir)
                        (unless lite-p (doom--module-paths))))))
        (total-success 0)
        (total-fail 0)
        (total-nocomp 0)
        (use-package-expand-minimally t))
    (let ((el-files (cl-loop for path in targets
                             if (file-directory-p path)
                               nconc (nreverse (directory-files-recursively path "\\.el$"))
                             else if (file-exists-p path)
                               collect path)))
      (dolist (file el-files)
        (when (or (not only-recompile-p)
                  (let ((elc-file (byte-compile-dest-file file)))
                    (and (file-exists-p elc-file)
                         (file-newer-than-file-p file elc-file))))
          (let ((result (if (string-match-p "/test/.+\\.el$" file)
                            'no-byte-compile
                          (byte-compile-file file)))
                (short-name (file-relative-name file doom-emacs-dir)))
            (cl-incf
             (cond ((eq result 'no-byte-compile)
                    (message! (dark (white "Ignored %s" short-name)))
                    total-nocomp)
                   ((null result)
                    (message! (red "Failed to compile %s" short-name))
                    total-fail)
                   (t
                    (message! (green "Compiled %s" short-name))
                    total-success))))))
      (message!
       (bold
        (color (if (= total-fail 0) 'green 'red)
               "%s %s file(s) %s"
               (if only-recompile-p "Recompiled" "Compiled")
               (format (if el-files "%d/%d" "%d")
                       total-success
                       (- (length el-files) total-nocomp))
               (format "(%s ignored)" total-nocomp)))))))

(defun doom/recompile ()
  "Recompile any out-of-date compiled *.el files in your Emacs configuration."
  (interactive)
  (doom/compile nil :recompile)
  ;; Forcibly recompile core.el in case `load-path' has changed
  (byte-recompile-file (expand-file-name "core.el" doom-core-dir) t))

(defun doom/reset ()
  "Clear the local cache completely (in `doom-cache-dir').

This resets Emacs to a blank slate. You must restart Emacs for some components
to feel its effects."
  (interactive)
  (delete-directory doom-cache-dir t)
  (make-directory doom-cache-dir t))

(defun doom/clean-compiled-files ()
  "Delete all compiled elc files in your Emacs configuration.

This excludes compiled packages in `doom-packages-dir'."
  (interactive)
  (let ((targets (append (list (expand-file-name "init.elc" doom-emacs-dir))
                         (directory-files-recursively doom-core-dir "\\.elc$")
                         (directory-files-recursively doom-modules-dir "\\.elc$"))))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                       collect path
                       and do (delete-file path)
                       and do (message "Deleted %s" (file-relative-name path)))
      (message "Everything is clean"))))


;;
;; Package.el modifications
;;

;; Updates QUELPA after deleting a package
(advice-add #'package-delete :after #'doom*package-delete)

;; It isn't safe to use `package-autoremove', so get rid of it
(advice-add #'package-autoremove :override #'doom/packages-autoremove)

(provide 'core-packages)
;;; core-packages.el ends here
