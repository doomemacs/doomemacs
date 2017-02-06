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
;;    more stable.
;; 4. No external dependencies (e.g. Cask) for plugin management.
;;
;; Note: it should be safe to use *most* package.el functions directly, but for
;; complete certainty I recommend the DOOM variants: `doom/install-package',
;; `doom/delete-package' and `doom/update-packages'. As well as:
;; `doom/packages-install', `doom/packages-update', and
;; `doom/packages-autoremove'.
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

(autoload 'use-package "use-package" nil nil 'macro)
(advice-add 'package-delete :after 'doom*package-delete)

(defmacro doom! (&rest packages)
  "DOOM Emacs bootstrap macro. List the modules to load. Benefits from
byte-compilation."
  (let (mode)
    (dolist (p packages)
      (cond ((keywordp p)
             (setq mode p))
            ((not mode)
             (error "No namespace specified on `doom!' for %s" p))
            (t
             (setq doom-modules (append doom-modules (list (cons mode p))))))))
  (unless noninteractive
    `(let (file-name-handler-alist)
       ,@(mapcar (lambda (pkg) `(load! ,(car pkg) ,(cdr pkg)))
                 doom-modules)

       (when (display-graphic-p)
         (require 'server)
         (unless (server-running-p)
           (server-start)))

       ;; Benchmark
       (format "Loaded %s packages in %s"
               (- (length load-path) (length doom--base-load-path))
               (emacs-init-time)))))

(defun doom-initialize (&optional force-p)
  "Initialize installed packages (using package.el) and ensure the core packages
are installed. If you byte compile core/core.el, calls to `package.el' are
avoided to speed up startup."
  (unless (or doom-init-p force-p)
    (setq load-path doom--base-load-path
          package-activated-list nil)
    (package-initialize t)
    ;; Sure, package-initialize fills the load-path, but it will error out on
    ;; missing packages. UNACCEPTAABBLLLE!
    (setq load-path (append load-path (directory-files package-user-dir t "^[a-zA-Z0-9]" t)))

    ;; Ensure cache folder exists
    (unless (file-exists-p doom-cache-dir)
      (make-directory doom-cache-dir t))

    (unless (and (file-exists-p doom-packages-dir)
                 (require 'use-package nil t)
                 (require 'quelpa nil t))
      (package-refresh-contents)
      ;; Ensure core packages are installed
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
  (unless (or (featurep 'autoloads)
              (load doom-autoload-file t t))
    (doom/refresh-autoloads)
    (unless (file-exists-p doom-autoload-file)
      (error "Autoloads file couldn't be generated"))))


;;
;; Macros
;;

(defvar __PACKAGE__ nil "The name of the current package.")

(defalias 'use-package! 'use-package
  "A `use-package' alias. It exists so DOOM configs adhere to the naming
conventions of DOOM emacs. Note that packages are deferred by default.

By DOOM conventions, using this instead of `package!' means you are configuring
a package regardless of whether it's installed or not, while `package!' is used
to declare how to install/setup a package.")

(defmacro package! (name &rest plist)
  "Declares a package. This does not load nor install them explicitly.

If used in `doom-core-dir', this is a wrapper for `use-package!' (all packages
are deferred by default), and takes the same arguments as `use-package'.

If used outside of `doom-core-dir' (i.e. in packages.el files within modules),
this macro serves a purely declarative purpose and doesn't call `use-package!'.
These calls are parsed by `doom-read-packages' to build `doom-packages'.

Adds a few custom properties in either case:

 :recipe RECIPE        Takes a MELPA-style recipe (see `quelpa-recipe' for an
                       example); for packages to be installed from external
                       sources.
 :pin ARCHIVE-NAME     Instructs ELPA to only look for this package in
                       ARCHIVE-NAME. e.g. \"org\".
 :needs FEATURE        Don't install this package if FEATURE isn't available. Can be a
                       (:module . submodule) cons pair.
 :setup CMD-OR-PCASE   A command to run after install. Can be a pcase list, whose
                       car's are symbols of OSes that `doom-os' returns, and whose
                       cdr's are string shell commands."
  (declare (indent defun))
  (mapc (lambda (key) (setq plist (use-package-plist-delete plist key)))
        '(:recipe :pin :setup :needs))
  `(let ((__PACKAGE__ ',name))
     (use-package! ,name ,@plist)))

(defmacro load! (module &optional submodule file)
  "Load a module from `doom-modules-dir' when both MODULE and SUBMODULE is
provided (both symbols). If FILE is non-nil, append it to the resulting path. If
SUBMODULE is nil, MODULE is loaded relative to the current file (see `__DIR__').
When SUBMODULE is nil, FILE isn't used.

Examples:
 (load! :lang emacs-lisp)

  Loads modules/lang/emacs-lisp/FILE.el (defaults to config.el).

 (load! +local-module)

  Loads +local-module.el relative to `__DIR__' or `doom-core-dir'."
  (let (path file)
    (cond ((null submodule)
           (setq path __DIR__
                 file (concat (symbol-name module) ".el")))
          (t
           (cl-pushnew (cons module submodule)
                       doom-modules
                       :test (lambda (x y) (and (eq (car x) (car y))
                                           (eq (cdr x) (cdr y)))))
           (setq path (doom-module-path module submodule)
                 file (or file "config.el"))))
    (setq path (f-slash path)
          file (concat path file))
    `(let ((__FILE__ ,file)
           (__DIR__  ,path))
       (load ,(f-no-ext file) nil (not doom-debug-mode)))))

(defun doom-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (setq module
        (cond ((keywordp module) (substring (symbol-name module) 1))
              ((symbolp module) (symbol-name module))
              ((stringp module) module)
              (t (error "Not a valid module name: %s" module))))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (f-expand (concat module "/" submodule "/" file)
            doom-modules-dir))


;;
;; Commands
;;

(defun doom/reload ()
  "Reload `load-path', `doom-modules' and `doom-packages' by
reinitializing doom and parsing config files for `package!' and `doom!' calls.
There are few reasons to use this."
  (interactive)
  (doom-initialize t)
  (doom-read-packages t)
  (doom-initialize-autoloads)
  (message "Reloaded %s packages" (length package-alist)))

(defun doom/refresh-autoloads ()
  "Refreshes the autoloads.el file, which tells Emacs where to find all the
autoloaded functions in the modules you use or among the core libraries, e.g.
core/autoload/*.el.

In modules, checks modules/*/autoload.el and modules/*/autoload/*.el.

Rerun this whenever init.el is modified. You can also use `make autoloads` from
the commandline."
  (interactive)
  (let ((generated-autoload-file doom-autoload-file)
        autoload-files)
    (setq autoload-files
          (append (-flatten (--map (let ((auto-dir  (f-expand "autoload" it))
                                         (auto-file (f-expand "autoload.el" it)))
                                     (cond ((f-directory-p auto-dir)
                                            (f-glob "*.el" auto-dir))
                                           ((f-exists-p auto-file)
                                            auto-file)))
                                   (--map (doom-module-path (car it) (cdr it))
                                          doom-modules)))
                  (f-glob "autoload/*.el" doom-core-dir)))
    (when (f-exists-p generated-autoload-file)
      (f-delete generated-autoload-file)
      (message "Deleted old autoloads.el"))
    (dolist (file autoload-files)
      (update-file-autoloads file)
      (message "Scanned %s" (f-relative file doom-emacs-dir)))
    (with-current-buffer (get-file-buffer generated-autoload-file)
      (save-buffer)
      (eval-buffer))
    (message "Done!")))

(defun doom/byte-compile (&optional simple-p)
  "Byte (re)compile the important files in your emacs configuration (init.el &
core/*.el). DOOM Emacs was designed to benefit from this.

If SIMPLE-P is nil, also byte-compile modules/*/*/*.el (except for packages.el).
There should be a measurable benefit from this, but it may take a while."
  (interactive)
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
                           doom-modules)))))
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

(provide 'core-packages)
;;; core-packages.el ends here
