;;; core-packages.el
;;
;; Emacs package management is opinionated. Unfortunately, so am I. So I
;; combined `use-package', `quelpa' and package.el to manage my plugins.
;;
;; Why all the trouble? Because:
;; 1. Scriptability: I want my plugins managable from the command line (as well
;;    as an `doom/packages-update' command within emacs to update my plugins
;;    automatically, rather than through package.el's interface).
;; 2. Flexibility: I want to install packages from sources other than ELPA
;;    repositories. Such as github or the Emacs wiki. Some plugins are out of
;;    date through official channels, have changed hands unofficially, or simply
;;    haven't been submitted to an ELPA repo yet.
;; 3. Stability: I don't want to worry that each time I use my package
;;    manager something might inexplicably go wrong. This was the case with
;;    Cask, which I used previously. package.el and quelpa appear to be much
;;    faster and more stable.
;; 4. No external dependencies (e.g. Cask) for plugin management.
;;
;; Note: it should be safe to use *most* package.el functions directly, though I
;; wouldn't recommend you use `package-autoremove'. For complete certainty, I've provided safer DOOM vaiants:
;; `doom/install-package', `doom/delete-package' and `doom/update-packages'.
;;
;; As well as: `doom/packages-install', `doom/packages-update', and
;; `doom/packages-autoremove', which are called from the Makefile.
;;
;; See core/autoload/packages.el for more functions.

(defvar doom-modules nil
  "List of enabled modules; each element is a cons cell (MODULE . SUBMODULE),
where MODULE is the module's property symbol, e.g. :lang, and SUBMODULE is the
submodule symbol, e.g. 'evil.")

(defvar doom-packages nil
  "A list of enabled packages.")

(defvar doom-protected-packages '(quelpa use-package dash f s)
  "A list of packages that shouldn't be deleted.")

(defvar doom-init-p nil
  "Non-nil if doom's package system has been initialized or not. It may not be
if you have byte-compiled your configuration (as intended).")

(defvar doom--base-load-path (append (list doom-core-dir
                                           doom-modules-dir)
                                     load-path)
  "A backup of `load-path', used as a bare-bones foundation for
`doom/packages-reload' or `doom-initialize'.")

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
are installed. If you byte compile core/core.el, calls to `package.el' are
avoided to speed up startup."
  ;; This is called early during Emacs initialization, so we can only use native
  ;; emacs functions.
  (unless (or doom-init-p force-p)
    (setq load-path doom--base-load-path
          package-activated-list nil)
    (package-initialize t)

    ;; Sure, `package-initialize' fills the load-path, but when NO-ACTIVATE is
    ;; non-nil, it will error out on missing packages. UNACCEPTAABBLLLE!
    (setq load-path (append load-path (directory-files package-user-dir t "^[a-zA-Z0-9]" t)))

    ;; Ensure cache folder exists
    (unless (file-exists-p doom-cache-dir)
      (make-directory doom-cache-dir t))

    ;; Ensure core packages are installed
    (unless (and (file-exists-p doom-packages-dir)
                 (require 'use-package nil t)
                 (require 'quelpa nil t))
      (package-refresh-contents)
      (condition-case ex
          (mapc (lambda (pkg)
                  (package-install pkg)
                  (unless (package-installed-p pkg)
                    (error "Couldn't install %s" pkg)))
                doom-protected-packages)
        (error
         (delete-directory doom-packages-dir t)
         (error "There was an error initializing DOOM. Try running it again"))))

    (require 'quelpa)
    (require 'use-package)
    ;; Remove package management keywords, I'll deal with the myself
    (mapc (lambda (keyword) (setq use-package-keywords (delq keyword use-package-keywords)))
          '(:ensure :pin))

    (setq doom-init-p t)))

(defun doom-initialize-autoloads (&optional force-p)
  "Ensures that an autoloads file exists and is loaded."
  (unless (ignore-errors (require 'autoloads doom-autoload-file t))
    (unless noninteractive
      (doom/reload-autoloads)
      (unless (file-exists-p doom-autoload-file)
        (error "Autoloads file couldn't be generated")))))

(defun doom-initialize-packages (&optional force-p)
  "Parses your Emacs config to keep track of packages declared with `@package'
in `doom-packages' and enabled modules in `doom-modules'."
  (doom-initialize force-p)
  (when (or force-p (not doom-modules) (not doom-packages))
    (setq doom-modules nil)
    (let ((noninteractive t))
      (mapc (lambda (file) (load file nil :nomessage))
            (list (f-expand "packages.el" doom-core-dir)
                  (f-expand "init.el" doom-emacs-dir)))
      ;; Look up packages.el for enabled modules
      (mapc (lambda (file) (load file :noerror :nomessage))
            (--map (doom-module-path (car it) (cdr it) "packages.el")
                   (doom-module-pairs))))))

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

(defun doom-module-pairs ()
  "TODO"
  (let (pairs module)
    (dolist (modules doom-modules)
      (setq module (car modules))
      (dolist (submodule (cdr modules))
        (push (cons module submodule) pairs)))
    pairs))

(defun doom-module-loaded-p (module submodule)
  "TODO"
  (memq submodule (cdr (assq module doom-modules))))

(defun doom-enable-module (module submodule &optional force-p)
  (unless (or force-p (doom-module-loaded-p module submodule))
    (let ((sublist (assq module doom-modules)))
      (if sublist
          (setf sublist (cons sublist submodule))
        (push (list module submodule) doom-modules)))))

(defun doom-enable-modules (modules)
  "TODO"
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m)
             (setq mode m))
            ((not mode)
             (error "No namespace specified on `@doom' for %s" m))
            ((eq m '*)
             (let ((mode-str (substring (symbol-name mode) 1)))
               (doom-enable-modules
                (cons mode
                      (--map (intern (f-base it))
                             (f-directories
                              (f-expand mode-str doom-modules-dir)))))))
            (t
             (doom-enable-module mode m))))
    doom-modules))


;;
;; Macros
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro @doom (&rest modules)
  "DOOM Emacs bootstrap macro. List the modules to load. Benefits from
byte-compilation."
  (doom-enable-modules modules)
  (unless noninteractive
    `(let (file-name-handler-alist)
       ,@(mapcar (lambda (pkg)
                   `(@require ,(car pkg) ,(cdr pkg) t))
                 (doom-module-pairs))

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
  "TODO"
  (let ((path (or (and path (eval path)) __DIR__))
        file)
    (unless path
      (error "Could not find %s" filesym))
    (setq file (f-expand (concat (symbol-name filesym) ".el") path))
    (if (f-exists-p file)
        `(let ((__FILE__ ,file)
               (__DIR__  ,path))
           (load ,(f-no-ext file) ,noerror (not doom-debug-mode)))
      (unless noerror
        (error "Could not @load file %s" file)))))

(defmacro @require (module submodule &optional reload-p)
  "Like `require', but for doom modules."
  (unless noninteractive
    (let ((loaded-p (doom-module-loaded-p module submodule)))
      (when (or reload-p (not loaded-p))
        (unless loaded-p
          (doom-enable-module module submodule t))
        `(@load config ,(doom-module-path module submodule) t)))))


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
  (doom-enable-module ,module ',submodule)
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
                                          (doom-module-pairs))))
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
                                       (and (f-ext-p it "el")
                                            (or (string= (f-base it) "config")
                                                (string-prefix-p "+" (f-base it))))
                                       t)
                           (doom-module-pairs))))))
        (n 0)
        results)
    (dolist (file targets)
      (push (cons (f-relative file doom-emacs-dir)
                  (when (byte-recompile-file file nil 0)
                    (setq n (1+ n))
                    t))
            results))
    (when noninteractive
      (when targets (message "\n"))
      (message "Compiled %s files:\n%s" n
               (mapconcat (lambda (file) (concat "+ " (if (cdr file) "SUCCESS" "FAIL") ": " (car file)))
                          (reverse results) "\n")))))


;;
;; Package.el modifications
;;

(advice-add 'package-delete :after 'doom*package-delete)

(provide 'core-packages)
;;; core-packages.el ends here
