;;; core-packages.el

;; Emacs package management is opinionated. Unfortunately, so am I. So with the
;; help of `use-package', `quelpa' and package.el, DOOM Emacs manages my
;; plugins and internal module dependency chains.
;;
;; So I've divided my config into two parts: configuration and
;; packages/dependency control. You'll find packages.el files in each DOOM
;; module (in `doom-modules-dir') and one in `doom-core-dir'. When executed,
;; they fill `doom-packages' and `doom-modules', which are necessary for DOOM to
;; extrapolate all kinds of information about plugins.
;;
;; Why all the trouble? Because:
;; 1. Scriptability: I want my plugins managable from the command line. This
;;    means a `doom/packages-update' command I can call to programmatically
;;    update my plugins, rather than through package.el's interface.
;; 2. Flexibility: I want to install packages from sources other than ELPA. Like
;;    github or the Emacs wiki. Some plugins are out-of-date through official
;;    channels, have changed hands unofficially, or simply haven't been
;;    submitted to an ELPA repo yet.
;; 3. Stability: I don't want to worry that each time I use my package
;;    manager something might inexplicably go wrong. Cask, which I used
;;    previously, was horribly unstable. package.el and quelpa, however, appear
;;    very stable (and much faster).
;; 4. Performance: A minor point, but it helps startup performance not to do
;;    package.el initialization and package installation checks at startup.
;; 5. Simplicity: Without Cask, I have no external dependencies to worry about
;;    (unless make counts). DOOM handles itself. Arguably, my emacs.d is
;;    overcomplicated, but configuring is simpler as a result (well, for me
;;    anyway :P).
;;
;; It should be safe to use package.el functionality, however, avoid
;; `package-autoremove' as it may not reliably select the right packages to
;; delete.
;;
;; For complete certainty, I've provided DOOM alternatives of package commands,
;; like `doom/install-package', `doom/delete-package' and
;; `doom/update-packages'. As well as: `doom/packages-install',
;; `doom/packages-update' and `doom/packages-autoremove', which are called from
;; the Makefile tasks.
;;
;; See core/autoload/packages.el for more functions.

(defvar doom-init-p nil
  "Non-nil if doom's package system has been initialized (by `doom-initialize').
This will be nil if you have byte-compiled your configuration (as intended).")

(defvar doom-modules nil
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-packages nil
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`@package' declaration. Set by `doom-initialize-packages'.")

(defvar doom-protected-packages
  '(persistent-soft quelpa use-package)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar doom-init-time nil
  "The time it took, in seconds, for DOOM Emacs to initialize.")

(defvar doom--base-load-path
  (append (list doom-core-dir doom-modules-dir)
          load-path)
  "A backup of `load-path' before it was altered by `doom-initialize'. Used as a
base when running `doom/reload', or by `@doom', for calculating how many
packages exist.")

(setq load-prefer-newer nil
      package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" doom-packages-dir)
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/"))
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.

      use-package-always-defer t
      use-package-always-ensure nil
      use-package-expand-minimally t
      use-package-debug nil
      use-package-verbose doom-debug-mode

      ;; Don't use MELPA, we'll use package.el for those
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-dir (expand-file-name "quelpa" doom-packages-dir)

      byte-compile-dynamic t
      byte-compile-warnings '(not mapcar free-vars unresolved noruntime lexical make-local))


;;
;; Bootstrap function
;;

(defun doom-initialize (&optional force-p)
  "Initialize installed packages (using package.el) and ensure the core packages
are installed. If you byte-compile core/core.el, this function will be avoided
to speed up startup."
  ;; Called early during initialization; only use native functions!
  (unless (or doom-init-p force-p)
    (setq load-path doom--base-load-path
          package-activated-list nil)

    ;; Ensure core folders exist
    (dolist (dir (list doom-local-dir doom-cache-dir package-user-dir))
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
    (setq load-path (append load-path (directory-files package-user-dir t "^[a-zA-Z0-9]" t)))

    ;; Ensure core packages are installed
    (let ((core-packages (cl-remove-if 'package-installed-p doom-protected-packages)))
      (when core-packages
        (package-refresh-contents)
        (dolist (pkg core-packages)
          (let ((inhibit-message t))
            (package-install pkg))
          (if (package-installed-p pkg)
              (message "Installed %s" pkg)
            (error "Couldn't install %s" pkg)))))

    (require 'quelpa)
    (require 'use-package)
    ;; Remove package management keywords, I'll deal with that myself
    (mapc (lambda (keyword) (setq use-package-keywords (delq keyword use-package-keywords)))
          '(:ensure :pin))

    (setq doom-init-p t)))

(defun doom-initialize-autoloads (&optional inhibit-reload-p)
  "Ensures that `doom-autoload-file' exists and is loaded. Otherwise run
`doom/reload-autoloads' to generate it."
  (unless (file-exists-p doom-autoload-file)
    (@quiet (doom/reload-autoloads))))

(defun doom-initialize-packages (&optional force-p load-p)
  "Crawls across your emacs.d in order to fill `doom-modules' (from init.el) and
`doom-packages' (from packages.el files), if they aren't set already. If FORCE-P
is non-nil, do it even if they are. Also aggressively loads all core autoload
files."
  (doom-initialize force-p)
  (let ((noninteractive t)
        (load-fn
         (lambda (file &optional noerror)
           (condition-case ex
               (load file noerror :nomessage :nosuffix)
             ('error (message "INIT-PACKAGES ERROR (%s): %s" file ex))))))
    (when (or force-p (not doom-modules))
      (setq doom-modules nil)
      (funcall load-fn (expand-file-name "init.el" doom-emacs-dir))
      (when load-p
        (mapc (lambda (file) (funcall load-fn file t))
              (append (reverse (file-expand-wildcards (concat doom-core-dir "core*.el")))
                      (file-expand-wildcards (concat doom-core-dir "autoload/*.el"))
                      (doom--module-paths "config.el")))))

    (when (or force-p (not doom-packages))
      (setq doom-packages nil)
      (funcall load-fn (expand-file-name "packages.el" doom-core-dir))
      (mapc (lambda (file) (funcall load-fn file t))
            (doom--module-paths "packages.el")))))

(defun doom-initialize-modules (modules)
  "Adds MODULES to `doom-modules'. MODULES must be in mplist format.

  e.g '(:feature evil :lang emacs-lisp javascript java)"
  (unless doom-modules
    (setq doom-modules (make-hash-table :test 'equal :size (length modules))))
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m)
             (setq mode m))
            ((not mode)
             (error "No namespace specified on `@doom' for %s" m))
            ((eq m '*)
             (doom-initialize-modules
              (cons mode
                    (mapcar
                     (lambda (dir) (intern (file-name-nondirectory dir)))
                     (cl-remove-if-not
                      'file-directory-p
                      (directory-files (expand-file-name
                                        (substring (symbol-name mode) 1)
                                        doom-modules-dir)
                                       t "^\\w"))))))
            (t
             (doom--enable-module mode m))))))

(defun doom-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (unless (keywordp module)
    (error "Expected a keyword, got %s" module))
  (unless (symbolp submodule)
    (error "Expected a symbol, got %s" submodule))
  (let ((module-name (substring (symbol-name module) 1))
        (submodule-name (symbol-name submodule)))
    (expand-file-name (concat module-name "/" submodule-name "/" file)
                      doom-modules-dir)))

(defun doom-module-loaded-p (module submodule)
  "Returns t if MODULE->SUBMODULE is present in `doom-modules'."
  (gethash (cons module submodule) doom-modules))

(defun doom--module-pairs ()
  "Returns `doom-modules' as a list of (MODULE . SUBMODULE) cons cells. The list
is sorted by order of insertion."
  (let (pairs)
    (maphash (lambda (key value) (push (cons (car key) (cdr key)) pairs))
             doom-modules)
    (reverse pairs)))

(defun doom--module-paths (&optional append-file)
  "Returns a list of absolute file paths to modules, with APPEND-FILE added, if
the file exists."
  (let (paths)
    (dolist (pair (doom--module-pairs))
      (let ((path (doom-module-path (car pair) (cdr pair) append-file)))
        (when (file-exists-p path)
          (push path paths))))
    (reverse paths)))

(defun doom--enable-module (module submodule &optional force-p)
  "Adds MODULE and SUBMODULE to `doom-modules', if it isn't already there (or if
FORCE-P is non-nil). MODULE is a keyword, SUBMODULE is a symbol. e.g. :lang
'emacs-lisp.

Used by `@require' and `@depends-on'."
  (unless (or force-p (doom-module-loaded-p module submodule))
    (puthash (cons module submodule) t doom-modules)))

(defun doom--display-benchmark ()
  (message "Loaded %s packages in %.03fs"
           (- (length load-path) (length doom--base-load-path))
           (setq doom-init-time (float-time (time-subtract after-init-time before-init-time)))))


;;
;; Macros
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro @doom (&rest modules)
  "DOOM Emacs bootstrap macro. List the modules to load. Benefits from
byte-compilation."
  (doom-initialize-modules modules)
  `(let (file-name-handler-alist)
     (setq doom-modules ',doom-modules)

     (unless noninteractive
       ,@(mapcar (lambda (module) `(@require ,(car module) ,(cdr module) t))
                 (doom--module-pairs))

       (when (display-graphic-p)
         (require 'server)
         (unless (server-running-p)
           (server-start)))

       ;; Benchmark
       (add-hook 'after-init-hook 'doom--display-benchmark t))))

(defmacro @def-package (name &rest plist)
  "Defines and configures a package using `use-package'. Packages are deferred
by default. If the package isn't installed or loaded, `@def-package' is
ignored."
  (when (or (featurep name)
            (package-installed-p name))
    `(use-package ,name ,@plist)))

(defmacro @load (filesym &optional path noerror)
  "Loads a file relative to the current module (or PATH). FILESYM is a file path
as a symbol. PATH is a directory to prefix it with. If NOERROR is non-nil, don't
throw an error if the file doesn't exist."
  (let ((path (or (and path
                       (cond ((symbolp path) (symbol-value path))
                             ((stringp path) path)
                             ((listp path) (eval path))))
                  (and load-file-name   (file-name-directory load-file-name))
                  (and buffer-file-name (file-name-directory buffer-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file)))))
    (unless path
      (error "Could not find %s" filesym))
    (let ((file (expand-file-name (concat (symbol-name filesym) ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror (not doom-debug-mode))
        (unless noerror
          (error "Could not @load file %s" file))))))

(defmacro @require (module submodule &optional reload-p)
  "Like `require', but for doom modules. Will load a module's config.el file if
it hasn't already, and if it exists."
  (when (or (not noninteractive)
            (bound-and-true-p byte-compile-current-file))
    (let ((loaded-p (doom-module-loaded-p module submodule)))
      (when (or reload-p (not loaded-p))
        (unless loaded-p
          (doom--enable-module module submodule t))
        `(@load config ,(doom-module-path module submodule) t)))))

(defmacro @featurep (module submodule)
  "Convenience macro that wraps `doom-module-loaded-p'."
  `(doom-module-loaded-p ,module ',submodule))


;;
;; Declarative macros
;;

(defmacro @package (name &rest plist)
  "Declares a package and how to install it (if applicable). This does not load
nor install them.

Accepts the following properties:

 :recipe RECIPE        Takes a MELPA-style recipe (see `quelpa-recipe' in
                       `quelpa' for an example); for packages to be installed
                       from external sources.
 :pin ARCHIVE-NAME     Instructs ELPA to only look for this package in
                       ARCHIVE-NAME. e.g. \"org\". Ignored if RECIPE is present.

This macro serves a purely declarative purpose, and are used to fill
`doom-packages', so that functions like `doom/packages-install' can operate on
them."
  (declare (indent defun))
  (let* ((old-plist (assq name doom-packages))
         (pkg-recipe (or (plist-get plist :recipe)
                         (and old-plist (plist-get old-plist :recipe))))
         (pkg-pin    (or (plist-get plist :pin)
                         (and old-plist (plist-get old-plist :pin)))))
    (when pkg-recipe
      (when (= 0 (mod (length pkg-recipe) 2))
        (plist-put plist :recipe (cons name pkg-recipe)))
      (when pkg-pin
        (plist-put plist :pin nil)))
    `(progn
       (when ,(and pkg-pin t)
         (cl-pushnew (cons ',name ,pkg-pin) package-pinned-packages :key 'car))
       (when ,(and old-plist t)
         (assq-delete-all ',name doom-packages))
       (push ',(cons name plist) doom-packages))))

(defmacro @depends-on (module submodule)
  "Declares that this module depends on another. MODULE is a keyword, and
SUBMODULE is a symbol."
  (doom--enable-module module submodule)
  `(@load packages ,(doom-module-path module submodule) t))


;;
;; Commands
;;

(defun doom/reload ()
  "Reload `load-path' by reinitializing package.el and reloading autoloads."
  (interactive)
  (doom-initialize t)
  (doom/reload-autoloads)
  (message "Reloaded %s packages" (length package-alist)))

(defun doom/reload-autoloads ()
  "Refreshes the autoloads.el file, which tells Emacs where to find all the
autoloaded functions in enabled modules or among the core libraries, e.g.
core/autoload/*.el.

In modules, checks modules/*/autoload.el and modules/*/autoload/*.el.

Rerun this whenever init.el is modified. You can also use `make autoloads` from
the commandline."
  (interactive)
  (doom-initialize-packages (not noninteractive))
  (let ((generated-autoload-file doom-autoload-file)
        (autoload-files
         (file-expand-wildcards
          (expand-file-name "autoload/*.el" doom-core-dir))))
    (dolist (path (doom--module-paths))
      (let ((auto-dir  (expand-file-name "autoload" path))
            (auto-file (expand-file-name "autoload.el" path)))
        (when (file-exists-p auto-file)
          (push auto-file autoload-files))
        (when (file-directory-p auto-dir)
          (mapc (lambda (file)
                  ;; Make evil.el autoload files a special case; don't load them
                  ;; unless evil is enabled.
                  (unless (and (equal (file-name-nondirectory file) "evil.el")
                               (not (@featurep :feature evil)))
                    (push file autoload-files)))
                (file-expand-wildcards (expand-file-name "*.el" auto-dir) t)))))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file)
      (message "Deleted old autoloads.el"))
    (dolist (file (reverse autoload-files))
      (let ((inhibit-message t))
        (update-file-autoloads file))
      (message "Scanned %s" (file-relative-name file doom-emacs-dir)))
    (condition-case ex
        (let ((buf (get-file-buffer generated-autoload-file)))
          (unwind-protect
              (with-current-buffer buf
                (save-buffer)
                (eval-buffer)
                (message "Finished generating autoloads.el!"))
            (kill-buffer buf)))
      ('error
       (delete-file generated-autoload-file)
       (error "Couldn't evaluate autoloads.el: %s" (cadr ex))))))

(defun doom/recompile ()
  "Byte (re)compile the important files in your emacs configuration (init.el,
core/*.el & modules/*/*/**.el). DOOM Emacs was designed to benefit from this.
This may take a while."
  (interactive)
  ;; Ensure all relevant config files are loaded. This way we don't need
  ;; eval-when-compile and require blocks scattered all over.
  (doom-initialize-packages (not noninteractive) noninteractive)
  (let ((targets
         (append (list (expand-file-name "init.el" doom-emacs-dir)
                       (expand-file-name "core.el" doom-core-dir))
                 (file-expand-wildcards (expand-file-name "core-*.el" doom-core-dir))
                 (file-expand-wildcards (expand-file-name "autoload/*.el" doom-core-dir))))
        (n 0)
        results)
    (dolist (path (doom--module-paths))
      (nconc targets (reverse (directory-files-recursively path "\\.el$"))))
    (dolist (file targets)
      (push (cons (file-relative-name file doom-emacs-dir)
                  (and (byte-recompile-file file nil 0)
                       (setq n (1+ n))))
            results))
    (when noninteractive
      (if targets (message "\n"))
      (message "Compiled %s files:\n%s" n
               (mapconcat (lambda (file) (concat "+ " (if (cdr file) "SUCCESS" "FAIL") ": " (car file)))
                          (reverse results) "\n")))))


;;
;; Package.el modifications
;;

(advice-add 'package-delete :after 'doom*package-delete)

(provide 'core-packages)
;;; core-packages.el ends here
