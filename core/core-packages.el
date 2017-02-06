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

(defvar doom-enabled-modules nil
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
      ;; ssh, no tears. Only compiling.
      byte-compile-warnings
      '(redefine callargs obsolete cl-functions interactive-only mapcar constants suspicious))


;;
;; Bootstrap function
;;

(autoload 'use-package "use-package" nil nil 'macro)

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
             (setq doom-enabled-modules (append doom-enabled-modules (list (cons mode p))))))))
  `(let (file-name-handler-alist)
     ,@(mapcar (lambda (pkg) `(load! ,(car pkg) ,(cdr pkg)))
               doom-enabled-modules)

     (when (display-graphic-p)
       (require 'server)
       (unless (server-running-p)
         (server-start)))

     ;; Prevent any auto-displayed text + benchmarking
     (advice-add 'display-startup-echo-area-message :override 'ignore)
     (message "Loaded %s packages in %s"
              (- (length load-path) (length doom--base-load-path))
              (emacs-init-time))))

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
    (setq load-path
          (append load-path
                  (directory-files package-user-dir t "^[a-zA-Z0-9]" t)))

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
    (advice-add 'package-delete :after 'doom*package-delete)
    ;; Remove package management keywords, I'll deal with the myself
    (mapc (lambda (keyword) (setq use-package-keywords (delq keyword use-package-keywords)))
          '(:ensure :pin))
    (setq doom-init-p t)))


;;
;; Macros
;;

(defvar __DIR__ nil  "The directory of the currently loaded file (set by `load!')")
(defvar __FILE__ nil "The full path of the currently loaded file (set by `load!')")
(defvar __PACKAGE__ nil "The name of the current package.")

(defun __DIR__ ()
  (or __DIR__
      (and load-file-name (f-dirname load-file-name))
      (and buffer-file-name (f-dirname buffer-file-name))
      default-directory
      (and (bound-and-true-p byte-compile-current-file)
           (f-dirname byte-compile-current-file))
      (error "__DIR__ is unset")))

(defun __FILE__ ()
  (or __FILE__
      load-file-name
      buffer-file-name
      (and (bound-and-true-p byte-compile-current-file)
           byte-compile-current-file)
      (error "__FILE__ is unset")))

(defmacro use-package! (name &rest plist)
  "A `use-package' wrapper. It exists so configs can adhere to the naming
conventions of DOOM emacs, as well as let-bind `__PACKAGE__' for the containing
forms. This is helpful for macros like `set!' and `add-hook!'. Note that
packages are deferred by default."
  (declare (indent defun))
  `(let ((__PACKAGE__ ',name))
     (use-package ,name ,@plist)))

(defmacro package! (name &rest plist)
  "Declares a package. This does not actually load nor install them explicitly.

If used in `doom-core-dir', this is a wrapper for `use-package!' (all packages
are deferred by default), and takes the same arguments as `use-package'.

If used outside of `doom-core-dir', this macro is purely declarative and doesn't
call `use-package!'. These calls are parsed by package management functions,
such as `doom-read-packages'.

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
  `(use-package! ,name ,@plist))

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
                       doom-enabled-modules
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
~/.emacs.d/modules/lang/emacs-lisp/. Will append FILE if non-nil."
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
  "Reload `load-path', `doom-enabled-modules' and `doom-packages' by
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
                                          doom-enabled-modules)))
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
                           doom-enabled-modules)))))
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
;; Package parsing
;;

(defun doom--parse-forms (sym forms)
  (let ((result-forms (and (boundp 'result-forms) result-forms)))
    (dolist (form forms)
      (cond ((eq (car-safe form) sym)
             (push (cdr-safe form) result-forms))

            ((and (listp form)
                  (not (-cons-pair? form)))
             (setq result-forms (doom--parse-forms sym form)))))
    result-forms))

(defun doom--parse-file-forms (sym file)
  (declare (indent defun))
  (unless (f-exists-p file)
    (error "%s does not exist" file))
  (unless (symbolp sym)
    (error "%s is not a valid symbol" sym))
  (let (forms)
    (with-temp-buffer
      (insert "(setq forms '(\n")
      (insert-file-contents file)
      (goto-char (point-max))
      (insert "\n))")
      (eval-buffer))
    (doom--parse-forms sym forms)))

(defun doom--strip-property (plist property)
  (let (forms)
    (while (and plist (not (eq (car plist) property)))
      (setq forms (append forms (list (pop plist)))))
    (pop plist)
    (while (and plist (not (keywordp (car plist))))
      (pop plist))
    (when plist
      (setq forms (append forms plist)))
    forms))

(defun doom-read-packages (&optional force-p nopackages)
  "Parses your Emacs config to keep track of packages declared with `package!'
in `doom-packages' and enabled modules in `doom-enabled-modules'."
  (doom-initialize)
  (when (or force-p (not doom-enabled-modules) (not doom-packages))
    (setq doom-enabled-modules
          (let (paths mode enabled-modules)
            (--each (doom--parse-file-forms 'doom! (f-expand "init.el" doom-emacs-dir))
              (dolist (module it)
                (cond ((keywordp module)
                       (setq mode module))
                      ((not mode)
                       (error "Malformed doom! call: no namespace for %s" module))
                      (t
                       (push (cons mode module) enabled-modules)))))
            enabled-modules))

    (unless nopackages
      (setq package-pinned-packages nil
            doom-packages nil)
      (mapc (lambda (pkg) (cl-pushnew pkg doom-packages :key 'car))
            (mapcar (lambda (args)
                      (mapc (lambda (keyword) (setq args (doom--strip-property args keyword)))
                            '(:preface :ensure :requires :no-require :bind :bind* :bind-keymap
                              :bind-keymap* :interpreter :mode :commands :defines :functions
                              :defer :init :after :demand :config :diminish :delight))
                      args)
                    (--sort (string-greaterp (symbol-name (car it))
                                             (symbol-name (car other)))
                            (-flatten-n
                             1 (mapcar (lambda (file)
                                         (when (f-exists-p file)
                                           (doom--parse-file-forms 'package! file)))
                                       (append (f-glob "core*.el" doom-core-dir)
                                               (--map (doom-module-path (car it) (cdr it) "packages.el")
                                                      doom-enabled-modules)))))))
      t)))

(provide 'core-packages)
;;; core-packages.el ends here
