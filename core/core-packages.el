;;; core-packages.el
;;
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
  "Non-nil if doom's package system has been initialized or not. It may not be
if you have byte-compiled your configuration (as intended). Running
`doom-initialize' sets this.")

(defvar doom-modules nil
  "Alist of enabled modules; each element is a list, whose CAR is a module
keyword, and whose CDR is a list of submodule symbols.")

(defvar doom-packages nil
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`@package' declaration.")

(defvar doom-protected-packages
  '(quelpa use-package dash f s)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

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

      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-dir (expand-file-name "quelpa" doom-packages-dir)

      byte-compile-dynamic t
      byte-compile-warnings '(not mapcar free-vars unresolved noruntime lexical make-local))


;;
;; Bootstrap function
;;

(defun doom-initialize (&optional force-p)
  "Initialize installed packages (using package.el) and ensure the core packages
are installed. If you byte compile core/core.el, this function will be avoided
to speed up startup."
  ;; This is called early during Emacs initialization, so we can only use native
  ;; emacs functions.
  (unless (or doom-init-p force-p)
    (setq load-path doom--base-load-path
          package-activated-list nil)

    ;; Ensure cache folder exists
    (mapc (lambda (dir)
            (unless (file-directory-p dir)
              (make-directory dir t)))
          (list doom-cache-dir package-user-dir))

    (package-initialize t)

    ;; Sure, `package-initialize' fills the load-path, but when NO-ACTIVATE is
    ;; non-nil, it will error out on missing packages. UNACCEPTAABBLLLE!
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

(defun doom-initialize-autoloads (&optional force-p)
  "Ensures that `doom-autoload-file' exists and is loaded. If it doesn't, run
`doom/reload-autoloads' to generate it."
  (unless (ignore-errors (require 'autoloads doom-autoload-file t))
    (unless noninteractive
      (doom/reload-autoloads)
      (unless (file-exists-p doom-autoload-file)
        (error "Autoloads file couldn't be generated")))))

(defun doom-initialize-packages (&optional force-p)
  "Executes the packages.el files across DOOM Emacs to refresh `doom-modules'
and `doom-packages'."
  (doom-initialize force-p)
  (when (or force-p (not doom-modules) (not doom-packages))
    (setq doom-modules nil
          doom-packages nil)
    (let ((noninteractive t))
      (mapc (lambda (file) (load file nil :nomessage))
            (list (f-expand "packages.el" doom-core-dir)
                  (f-expand "init.el" doom-emacs-dir)))
      (mapc (lambda (file) (load file :noerror :nomessage))
            (--map (doom-module-path (car it) (cdr it) "packages.el")
                   (doom--module-pairs))))))

(defun doom-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (unless (keywordp module)
    (error "Expected a keyword, got %s" module))
  (unless (symbolp submodule)
    (error "Expected a symbol, got %s" submodule))
  (let ((module-name (substring (symbol-name module) 1))
        (submodule-name (symbol-name submodule)))
    (f-expand (concat module-name "/" submodule-name "/" file)
              doom-modules-dir)))

(defun doom-module-loaded-p (module submodule)
  "Returns t if SUBMODULE (in MODULE) is present in `doom-modules'."
  (memq submodule (cdr (assq module doom-modules))))

(defun doom--module-pairs ()
  "Returns `doom-modules' as a list of (MODULE . SUBMODULE) cons cells."
  (let (pairs module)
    (dolist (modules doom-modules)
      (setq module (car modules))
      (dolist (submodule (cdr modules))
        (push (cons module submodule) pairs)))
    pairs))

(defun doom--enable-module (module submodule &optional force-p)
  "Adds MODULE and SUBMODULE to `doom-modules', if it isn't already there (or if
FORCE-P is non-nil). MODULE is a keyword, SUBMODULE is a symbol. e.g. :lang
'emacs-lisp.

Used by `@require' and `@depends-on'."
  (unless (or force-p (doom-module-loaded-p module submodule))
    (let ((sublist (assq module doom-modules)))
      (if sublist
          (setcdr (last sublist) (list submodule))
        (push (list module submodule) doom-modules)))))

(defun doom--enable-modules (modules)
  "Adds MODULES to `doom-modules'.

MODULES must be in mplist format:
  '(:feature evil :lang emacs-lisp javascript java)"
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m)
             (setq mode m))
            ((not mode)
             (error "No namespace specified on `@doom' for %s" m))
            ((eq m '*)
             (let ((mode-str (substring (symbol-name mode) 1)))
               (doom--enable-modules
                (cons mode
                      (--map (intern (f-base it))
                             (f-directories
                              (f-expand mode-str doom-modules-dir)))))))
            (t
             (doom--enable-module mode m))))
    doom-modules))


;;
;; Macros
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro @doom (&rest modules)
  "DOOM Emacs bootstrap macro. List the modules to load. Benefits from
byte-compilation."
  (doom--enable-modules modules)
  (unless noninteractive
    `(let (file-name-handler-alist)
       (setq doom-modules ',doom-modules)

       ,@(mapcar (lambda (pkg)
                   `(@require ,(car pkg) ,(cdr pkg) t))
                 (doom--module-pairs))

       (when (display-graphic-p)
         (require 'server)
         (unless (server-running-p)
           (server-start)))

       ;; Benchmark
       (format "Loaded %s packages in %s"
               (- (length load-path) (length doom--base-load-path))
               (emacs-init-time)))))

(defalias '@def-package 'use-package
  "A `use-package' alias. It exists so DOOM configs adhere to the naming
conventions of DOOM emacs. Note that packages are deferred by default.")

(defmacro @load (filesym &optional path noerror)
  "Loads a file relative to the current module (or PATH). FILESYM is a file path
as a symbol. PATH is a directory to prefix it with. If NOERROR is non-nil, don't
throw an error if the file doesn't exist.

Sets `__FILE__' and `__DIR__' on the loaded file."
  (let ((path (or (and path (eval path)) __DIR__)))
    (unless path
      (error "Could not find %s" filesym))
    (let ((file (f-expand (concat (symbol-name filesym) ".el") path)))
      (if (f-exists-p file)
          `(let ((__FILE__ ,file)
                 (__DIR__  ,path))
             (load ,(f-no-ext file) ,noerror (not doom-debug-mode)))
        (unless noerror
          (error "Could not @load file %s" file))))))

(defmacro @require (module submodule &optional reload-p)
  "Like `require', but for doom modules. Will load a module's config.el file if
it hasn't already, and if it exists."
  (unless noninteractive
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
  "Declares a package. This does not load nor install them explicitly.

this macro serves a purely declarative purpose, and are run to build
`doom-packages', so that functions like `doom/packages-install' can operate on
them.

Accepts the following properties:

 :recipe RECIPE        Takes a MELPA-style recipe (see `quelpa-recipe' for an
                       example); for packages to be installed from external
                       sources.
 :pin ARCHIVE-NAME     Instructs ELPA to only look for this package in
                       ARCHIVE-NAME. e.g. \"org\"."
  (declare (indent defun))
  (let ((pkg-recipe (plist-get plist :recipe))
        (pkg-pin    (plist-get plist :pin)))
    (when (= 0 (mod (length pkg-recipe) 2))
      (plist-put plist :recipe (cons name pkg-recipe)))
    `(add-to-list 'doom-packages ',(cons name plist) t)))

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
autoloaded functions in the modules you use or among the core libraries, e.g.
core/autoload/*.el.

In modules, checks modules/*/autoload.el and modules/*/autoload/*.el.

Rerun this whenever init.el is modified. You can also use `make autoloads` from
the commandline."
  (interactive)
  (doom-initialize-packages noninteractive)
  (let ((generated-autoload-file doom-autoload-file)
        autoload-files)
    (setq autoload-files
          (append (-flatten (--map (let ((auto-dir  (f-expand "autoload" it))
                                         (auto-file (f-expand "autoload.el" it)))
                                     (append (and (f-exists-p auto-file)
                                                  (list auto-file))
                                             (and (f-directory-p auto-dir)
                                                  (f-glob "*.el" auto-dir))))
                                   (--map (doom-module-path (car it) (cdr it))
                                          (doom--module-pairs))))
                  (f-glob "autoload/*.el" doom-core-dir)))
    (when (f-exists-p generated-autoload-file)
      (f-delete generated-autoload-file)
      (message "Deleted old autoloads.el"))
    (dolist (file autoload-files)
      (@quiet (update-file-autoloads file))
      (message "Scanned %s" (f-relative file doom-emacs-dir)))
    (condition-case ex
        (with-current-buffer (get-file-buffer generated-autoload-file)
          (save-buffer)
          (eval-buffer)
          (message "Done!"))
      ('error (error "Couldn't evaluate autoloads.el: %s" (cadr ex))))))

(defun doom/recompile (&optional simple-p)
  "Byte (re)compile the important files in your emacs configuration (init.el &
core/*.el). DOOM Emacs was designed to benefit from this.

If SIMPLE-P is nil, also byte-compile modules/*/*/*.el (except for packages.el).
There should be a measurable benefit from this, but it may take a while."
  (interactive)
  (doom-initialize-packages t)
  (let ((targets
         (append (list (f-expand "init.el" doom-emacs-dir)
                       (f-expand "core.el" doom-core-dir))
                 (f-glob "core-*.el" doom-core-dir)
                 (unless simple-p
                   (-flatten
                    (--map (f--entries (doom-module-path (car it) (cdr it))
                                       (f-ext-p it "el") t)
                           (doom--module-pairs))))))
        (n 0)
        results)
    (dolist (file targets)
      (push (cons (f-relative file doom-emacs-dir)
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
