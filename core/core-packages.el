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
;; You should be able to use package.el commands without any conflicts, but to
;; be absolutely certain use the doom alternatives:
;;
;;    + `package-install':          `doom/install-package'
;;    + `package-reinstall':        `doom/reinstall-package'
;;    + `package-delete':           `doom/delete-package'
;;    + `package-update':           `doom/update-package'
;;    + `package-autoremove':       `doom//packages-autoremove'
;;    + `package-refresh-contents': `doom/refresh-packages'
;;
;; See core/autoload/packages.el for more functions.

(defvar doom-init-p nil
  "Non-nil if doom is done initializing (once `doom-post-init-hook' is done). If
this is nil after Emacs has started something is wrong.")

(defvar doom-init-time nil
  "The time it took, in seconds, for DOOM Emacs to initialize.")

(defvar doom-modules ()
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

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

(defvar doom--site-load-path load-path
  "The load path of built in Emacs libraries.")

(defvar doom--package-load-path ()
  "The load path of package libraries installed via ELPA and QUELPA.")

(defvar doom--base-load-path
  (append (list doom-core-dir doom-modules-dir)
          doom--site-load-path)
  "A backup of `load-path' before it was altered by `doom-initialize'. Used as a
base by `doom!' and for calculating how many packages exist.")

(defvar doom--refreshed-p nil)

(setq package--init-file-ensured t
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
are installed.

If you byte-compile core/core.el, this function will be avoided to speed up
startup."
  ;; Called early during initialization; only use native (and cl-lib) functions!
  (when (or force-p (not doom-init-p))
    ;; Speed things up with a `load-path' for only the bare essentials
    (let ((load-path doom--base-load-path))
      ;; Ensure core folders exist, otherwise we get errors
      (dolist (dir (list doom-local-dir doom-etc-dir doom-cache-dir doom-packages-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      ;; Ensure package.el is initialized; we use its state
      (setq package-activated-list nil)
      (condition-case _ (package-initialize t)
        ('error (package-refresh-contents)
                (setq doom--refreshed-p t)
                (package-initialize t)))
      ;; Ensure core packages are installed
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
      (setq doom-init-p t))))

(defun doom-initialize-load-path (&optional force-p)
  (when (or force-p (not doom--package-load-path))
    ;; We could let `package-initialize' fill `load-path', but it does more than
    ;; that alone (like load autoload files). If you want something prematurely
    ;; optimizated right, ya gotta do it yourself.
    ;;
    ;; Also, in some edge cases involving package initialization during a
    ;; non-interactive session, `package-initialize' fails to fill `load-path'.
    (setq doom--package-load-path (directory-files package-user-dir t "^[^.]" t)
          load-path (append doom--base-load-path doom--package-load-path))))

(defun doom-initialize-autoloads ()
  "Ensures that `doom-autoload-file' exists and is loaded. Otherwise run
`doom/reload-autoloads' to generate it."
  (unless (file-exists-p doom-autoload-file)
    (quiet! (doom//reload-autoloads))))

(defun doom-initialize-packages (&optional force-p load-p)
  "Crawls across your emacs.d to fill `doom-modules' (from init.el) and
`doom-packages' (from packages.el files), if they aren't set already.

If FORCE-P is non-nil, do it even if they are.

This aggressively reloads core autoload files."
  (doom-initialize-load-path force-p)
  (with-temp-buffer ; prevent buffer-local settings from propagating
    (cl-flet
        ((_load
          (file &optional noerror interactive)
          (condition-case-unless-debug ex
              (let ((load-prefer-newer t)
                    (noninteractive (not interactive)))
                (load file noerror :nomessage :nosuffix))
            ('error
             (lwarn 'doom-initialize-packages :warning
                    "%s in %s: %s"
                    (car ex)
                    (file-relative-name file doom-emacs-dir)
                    (error-message-string ex))))))
      (when (or force-p (not doom-modules))
        (setq doom-modules nil
              doom-packages nil)
        (_load (concat doom-core-dir "core.el") nil 'interactive)
        (_load (expand-file-name "init.el" doom-emacs-dir))
        (when load-p
          (mapc #'_load (file-expand-wildcards (expand-file-name "autoload/*.el" doom-core-dir)))
          (_load (expand-file-name "init.el" doom-emacs-dir) nil 'interactive)))
      (when (or force-p (not doom-packages))
        (setq doom-packages nil)
        (_load (expand-file-name "packages.el" doom-core-dir))
        (cl-loop for (module . submodule) in (doom-module-pairs)
                 for path = (doom-module-path module submodule "packages.el")
                 do (_load path 'noerror))))))

(defun doom-initialize-modules (modules)
  "Adds MODULES to `doom-modules'. MODULES must be in mplist format.

  e.g '(:feature evil :lang emacs-lisp javascript java)"
  (unless doom-modules
    (setq doom-modules (make-hash-table :test #'equal
                                        :size (+ 5 (length modules))
                                        :rehash-threshold 1.0)))
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m) (setq mode m))
            ((not mode)   (error "No namespace specified on `doom!' for %s" m))
            ((listp m)    (doom-module-enable mode (car m) (cdr m)))
            (t            (doom-module-enable mode m))))))

(defun doom-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (when (keywordp module)
    (setq module (substring (symbol-name module) 1)))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (expand-file-name (concat module "/" submodule "/" file)
                    doom-modules-dir))

(defun doom-module-from-path (path)
  "Get module cons cell (MODULE . SUBMODULE) for PATH, if possible."
  (when-let* ((path (file-relative-name (file-truename path) (file-truename doom-modules-dir))))
    (let ((segments (split-string path "/")))
      (cons (intern (concat ":" (car segments)))
            (intern (cadr segments))))))

(defun doom-module-paths (&optional append-file)
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added, if the file exists."
  (cl-loop for (module . submodule) in (doom-module-pairs)
           for path = (doom-module-path module submodule append-file)
           if (file-exists-p path)
           collect path))

(defun doom-module-get (module submodule)
  "Returns a list of flags provided for MODULE SUBMODULE."
  (gethash (cons module submodule) doom-modules))

(defun doom-module-enabled-p (module submodule)
  "Returns t if MODULE->SUBMODULE is present in `doom-modules'."
  (and (doom-module-get module submodule) t))

(defun doom-module-enable (module submodule &optional flags)
  "Adds MODULE and SUBMODULE to `doom-modules', overwriting it if it exists.

MODULE is a keyword, SUBMODULE is a symbol. e.g. :lang 'emacs-lisp.

Used by `require!' and `depends-on!'."
  (let ((key (cons module submodule)))
    (puthash key
             (or (doom-enlist flags)
                 (gethash key doom-modules)
                 '(t))
             doom-modules)))

(defun doom-module-pairs ()
  "Returns `doom-modules' as a list of (MODULE . SUBMODULE) cons cells. The list
is sorted by order of insertion unless ALL-P is non-nil. If ALL-P is non-nil,
include all modules, enabled or otherwise."
  (unless (hash-table-p doom-modules)
    (error "doom-modules is uninitialized"))
  (cl-loop for key being the hash-keys of doom-modules
           collect key))

(defun doom-packages--display-benchmark ()
  (message "Doom loaded %s packages across %d modules in %.03fs"
           ;; Certainly imprecise, especially where custom additions to
           ;; load-path are concerned, but I don't mind a [small] margin of
           ;; error in the plugin count in exchange for faster startup.
           (length doom--package-load-path)
           (hash-table-size doom-modules)
           (setq doom-init-time (float-time (time-subtract after-init-time before-init-time)))))


;;
;; Macros
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro doom! (&rest modules)
  "Bootstrap DOOM Emacs.

MODULES is an malformed plist of modules to load."
  (doom-initialize-modules modules)
  `(let (file-name-handler-alist)
     (setq doom-modules ',doom-modules)
     (unless noninteractive
       (message "Doom initialized")
       ,@(cl-loop for (module . submodule) in (doom-module-pairs)
                  for module-path = (doom-module-path module submodule)
                  collect `(load! init ,module-path t) into inits
                  collect `(load! config ,module-path t) into configs
                  finally return (append inits configs))
       (when (display-graphic-p)
         (require 'server)
         (unless (server-running-p)
           (server-start)))
       (add-hook 'doom-init-hook #'doom-packages--display-benchmark t)
       (message "Doom modules initialized"))))

(defmacro def-package! (name &rest plist)
  "A thin wrapper around `use-package'."
  ;; Ignore package if NAME is in `doom-disabled-packages'
  (when (and (memq name doom-disabled-packages)
             (not (memq :disabled plist)))
    (setq plist `(:disabled t ,@plist)))
  ;; If byte-compiling, ignore this package if it doesn't meet the condition.
  ;; This avoids false-positive load errors.
  (unless (and (bound-and-true-p byte-compile-current-file)
               (or (and (plist-member plist :if)     (not (eval (plist-get plist :if))))
                   (and (plist-member plist :when)   (not (eval (plist-get plist :when))))
                   (and (plist-member plist :unless) (eval (plist-get plist :unless)))))
    `(use-package ,name ,@plist)))

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
where to look for the file (a string representing a directory path). If omitted,
the lookup is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (cl-assert (symbolp filesym) t)
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

(defmacro require! (module submodule &optional flags reload-p)
  "Loads the module specified by MODULE (a property) and SUBMODULE (a symbol).

The module is only loaded once. If RELOAD-P is non-nil, load it again."
  (when (or reload-p (not (doom-module-enabled-p module submodule)))
    (let ((module-path (doom-module-path module submodule)))
      (if (not (file-directory-p module-path))
          (lwarn 'doom-modules :warning "Couldn't find module '%s %s'"
                 module submodule)
        (doom-module-enable module submodule flags)
        `(condition-case-unless-debug ex
             (load! config ,module-path t)
           ('error
            (lwarn 'doom-modules :error
                   "%s in '%s %s' -> %s"
                   (car ex) ,module ',submodule
                   (error-message-string ex))))))))

(defmacro featurep! (module &optional submodule flag)
  "A convenience macro wrapper for `doom-module-enabled-p'. It is evaluated at
compile-time/macro-expansion time."
  (unless submodule
    (let* ((path (or load-file-name byte-compile-current-file))
           (module-pair (doom-module-from-path path)))
      (unless module-pair
        (error "featurep! couldn't detect what module I'm in! (in %s)" path))
      (setq flag module
            module (car module-pair)
            submodule (cdr module-pair))))
  (if flag
      (and (memq flag (doom-module-get module submodule)) t)
    (doom-module-enabled-p module submodule)))


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
      (when-let* ((val (plist-get plist prop)))
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
  (doom-module-enable module submodule)
  `(load! packages ,(doom-module-path module submodule) t))


;;
;; Commands
;;

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
  (let* ((default-directory doom-emacs-dir)
         (compilation-filter-hook
          (list (lambda () (ansi-color-apply-on-region compilation-filter-start (point))))))
    (compile (format "%s --quick --batch -l core/core.el -f %s"
                     (executable-find "emacs")
                     (symbol-name fn)))
    (while compilation-in-progress
      (sit-for 1))))

(defun doom//reload-load-path ()
  "Reload `load-path' and recompile files (if necessary).

Use this when `load-path' is out of sync with your plugins. This should only
happen if you manually modify/update/install packages from outside Emacs, while
an Emacs session is running.

This isn't necessary if you use Doom's package management commands because they
call `doom/reload-load-path' remotely (through emacsclient)."
  (interactive)
  (byte-recompile-file (expand-file-name "core.el" doom-core-dir) t)
  (cond (noninteractive
         (require 'server)
         (when (server-running-p)
           (message "Reloading active Emacs session...")
           (server-eval-at server-name '(doom//reload-load-path))))
        ((let ((noninteractive t))
           (doom-initialize-load-path t)
           (message "%d packages reloaded" (length doom--package-load-path))
           (run-hooks 'doom-reload-hook)))))

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
      ;; This is done in another instance to protect the current session's
      ;; state. `doom-initialize-packages' will have side effects otherwise.
      (and (doom-packages--async-run 'doom//reload-autoloads)
           (load doom-autoload-file))
    (doom-initialize-packages t)
    (let ((targets
           (file-expand-wildcards
            (expand-file-name "autoload/*.el" doom-core-dir))))
      (dolist (path (doom-module-paths))
        (let ((auto-dir  (expand-file-name "autoload" path))
              (auto-file (expand-file-name "autoload.el" path)))
          (when (file-exists-p auto-file)
            (push auto-file targets))
          (when (file-directory-p auto-dir)
            (dolist (file (directory-files-recursively auto-dir "\\.el$"))
              (push file targets)))))
      (when (file-exists-p doom-autoload-file)
        (delete-file doom-autoload-file)
        (message "Deleted old autoloads.el"))
      (dolist (file (reverse targets))
        (message
         (cond ((not (doom-packages--read-if-cookies file))
                "⚠ Ignoring %s")
               ((update-file-autoloads file nil doom-autoload-file)
                "✕ Nothing in %s")
               (t
                "✓ Scanned %s"))
         (file-relative-name file doom-emacs-dir)))
      (make-directory (file-name-directory doom-autoload-file) t)
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
        (recompile-p (or recompile-p
                         (and (member "-r" (cdr argv)) t))))
    (if (not noninteractive)
        ;; This is done in another instance to protect the current session's
        ;; state. `doom-initialize-packages' will have side effects otherwise.
        (doom-packages--async-run 'doom//byte-compile)
      (let ((total-ok   0)
            (total-fail 0)
            (total-noop 0)
            (modules (or modules (cdr argv)))
            compile-targets)
        (doom-initialize-packages t t)
        (setq compile-targets
              (cl-loop for target
                       in (or modules (append (list doom-core-dir) (doom-module-paths)))
                       if (equal target "core")
                        nconc (nreverse (directory-files-recursively doom-core-dir "\\.el$"))
                       else if (file-directory-p target)
                        nconc (nreverse (directory-files-recursively target "\\.el$"))
                       else if (file-directory-p (expand-file-name target doom-modules-dir))
                        nconc (nreverse (directory-files-recursively (expand-file-name target doom-modules-dir) "\\.el$"))
                       else if (file-exists-p target)
                        collect target
                       finally do (setq argv nil)))
        (unless compile-targets
          (error "No targets to compile"))
        (let ((use-package-expand-minimally t))
          (push (expand-file-name "init.el" doom-emacs-dir) compile-targets)
          (condition-case ex
              (progn
                (dolist (target compile-targets)
                  (when (or (not recompile-p)
                            (let ((elc-file (byte-compile-dest-file target)))
                              (and (file-exists-p elc-file)
                                   (file-newer-than-file-p file elc-file))))
                    (let ((result (if (doom-packages--read-if-cookies target)
                                      (byte-compile-file target)
                                    'no-byte-compile))
                          (short-name (file-relative-name target doom-emacs-dir)))
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
             (doom//clean-byte-compiled-files)
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
  (byte-recompile-directory package-user-dir 0 t))

(defun doom//clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration. This excludes
compiled packages.'"
  (interactive)
  (let ((targets (append (list (expand-file-name "init.elc" doom-emacs-dir))
                         (directory-files-recursively doom-core-dir "\\.elc$")
                         (directory-files-recursively doom-modules-dir "\\.elc$")))
        (default-directory doom-emacs-dir))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path)
                     and do (message "✓ Deleted %s" (file-relative-name path)))
      (message "Everything is clean"))))


;;
;; Package.el modifications
;;

;; Updates QUELPA after deleting a package
(advice-add #'package-delete :after #'doom*package-delete)

;; It isn't safe to use `package-autoremove', so get rid of it
(advice-add #'package-autoremove :override #'doom//packages-autoremove)

(provide 'core-packages)
;;; core-packages.el ends here
