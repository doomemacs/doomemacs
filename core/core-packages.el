;;; core-packages.el

;; Emacs package management is opinionated. Unfortunately, so am I. So I
;; combined `use-package`, quelpa and package.el to manage my plugins.
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

(defvar doom-dont-load-p nil
  "If non-nil, run DOOM emacs in declarative mode, meaning: don't actually load
anything, just track what should be loaded. Useful for scanning packages and
loaded modules.")

(defvar doom-reloading-p nil
  "If non-nil, DOOM is reloading itself. Use this to determine to prevent
infinite recursion.")

(defvar doom-prefer-el-p noninteractive
  "If non-nil, load uncompiled .el config files.")

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
      '(unresolved callargs obsolete noruntime cl-functions make-local constants suspicious))


;;
;; Bootstrap function
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro doom! (&rest packages)
  "DOOM Emacs bootstrap macro. List the modules to load. Benefits from
byte-compilation."
  (let (mode)
    (dolist (p packages)
      (cond ((string-prefix-p ":" (symbol-name p))
             (setq mode p))
            ((not mode)
             (error "No namespace specified on `doom!' for %s" p))
            (t
             (setq doom-enabled-modules (append doom-enabled-modules (list (cons mode p))))))))
  `(unless doom-dont-load-p
     (let (file-name-handler-alist)
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

(defun doom-reload ()
  "Rereads the Emacs config, reloading `doom-packages' and
`doom-enabled-modules'."
  (unless doom-reloading-p
    (doom-initialize)
    (let ((doom-prefer-el-p t)
          (doom-dont-load-p t)
          (doom-reloading-p t)
          noninteractive)
      (setq doom-enabled-modules nil
            doom-packages nil)
      (let ((load-fn (lambda (file) (load file :noerror (not doom-debug-mode) :nosuffix))))
        (mapc load-fn
              (list (f-expand "init.el" doom-emacs-dir)
                    (f-expand "core.el" doom-core-dir)))
        (mapc load-fn
              (--map (doom-module-path (car it) (cdr it) "packages.el")
                     doom-enabled-modules))))))


;;
;; Macros
;;

(defvar __DIR__ nil  "The directory of the currently loaded file (set by `load!')")
(defvar __FILE__ nil "The full path of the currently loaded file (set by `load!')")
(defvar __PACKAGE__ nil "The name of the current package.")

(defmacro use-package! (name &rest plist)
  "A `use-package' wrapper. It exists so configs can adhere to the naming
conventions of DOOM emacs, as well as let-bind `__PACKAGE__' for the containing
forms. This is helpful for macros like `set!' and `add-hook!'. Note that
packages are deferred by default."
  (declare (indent defun))
  `(let ((__PACKAGE__ ',name))
     (use-package ,name ,@plist)))

(defmacro package! (name &rest plist)
  "Wraps around `use-package' to declare a deferred package (unless otherwise
indicated), takes the same arguments, but adds a few custom properties:

 :recipe RECIPE        Takes a MELPA-style recipe (see `quelpa-recipe' for an
                       example); for packages to be installed from external
                       sources.
 :pin ARCHIVE-NAME     Instructs ELPA to only look for this package in
                       ARCHIVE-NAME. e.g. \"org\".
 :needs FEATURE        Don't install this package if FEATURE isn't available.

Also binds `__PACKAGE__` for PLIST forms to optionally use."
  (declare (indent defun))
  (let ((recipe (cadr (memq :recipe plist)))
        (pin    (cadr (memq :pin plist)))
        (lpath  (cadr (memq :load-path plist)))
        (dep    (cadr (memq :needs plist))))
    (when (or (not dep)
              (or (featurep dep)
                  (package-installed-p dep)))
      (when (and recipe (= 0 (mod (length recipe) 2)))
        (push name recipe))
      (if (not lpath)
          (cl-pushnew (cons name recipe) doom-packages :key 'car)
        (cl-pushnew lpath doom--base-load-path)
        (setq recipe nil
              pin nil))
      (when pin
        (cl-pushnew (cons package (plist-get plist :pin)) package-pinned-packages :key 'car))
      (setq plist (use-package-plist-delete plist :recipe))
      (setq plist (use-package-plist-delete plist :pin))
      (unless doom-dont-load-p
        `(use-package! ,name ,@plist)))))

(defmacro require! (feature)
  "Like `require', but will prefer uncompiled files if `doom-prefer-el-p' is
non-nil or this is a noninteractive session."
  (let ((prefer-el-p (or doom-prefer-el-p noninteractive)))
    `(require ',feature
              ,(locate-file (concat (symbol-name feature) (if prefer-el-p ".el"))
                            load-path))))

(defmacro load! (module &optional submodule file)
  "Load a module from `doom-modules-dir' when both MODULE and SUBMODULE is
provided (both symbols). If FILE is non-nil, append it to the resulting path. If
SUBMODULE is nil, MODULE is loaded relative to the current file (see `__DIR__').
When SUBMODULE is nil, FILE isn't used.

Will prefer uncompiled elisp sources if `doom-prefer-el-p' is non-nil or this is
an noninteractive session.

Examples:
 (load! :lang emacs-lisp)

  Loads modules/lang/emacs-lisp/FILE.el (defaults to config.el).

 (load! +local-module)

  Loads +local-module.el relative to `__DIR__' or `doom-core-dir'."
  (let ((prefer-el-p (or doom-prefer-el-p noninteractive))
        path file)
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
       (load ,(if doom-prefer-el-p file (f-no-ext file))
             nil (not doom-debug-mode) ,doom-prefer-el-p))))

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
  "Reload `load-path' by scanning all packages. Run this if you ran make update
or make clean outside of Emacs."
  (interactive)
  (doom-initialize t)
  (message "Reloaded %s packages" (length package-alist)))

(defun doom/refresh-autoloads ()
  "Refreshes the autoloads.el file, which tells Emacs where to find all the
autoloaded functions in the modules you use or among the core libraries.

In modules, checks for modules/*/autoload.el and modules/*/autoload/*.el.

Rerun this whenever you modify your init.el (or use `make autoloads` from the
command line)."
  (interactive)
  (unless doom-reloading-p
    (doom-reload)
    (let ((generated-autoload-file (concat doom-local-dir "autoloads.el"))
          (autoload-files
           (append (-flatten (mapcar (lambda (dir)
                                       (let ((auto-dir  (f-expand "autoload" dir))
                                             (auto-file (f-expand "autoload.el" dir)))
                                         (cond ((f-directory-p auto-dir)
                                                (f-glob "*.el" auto-dir))
                                               ((f-exists-p auto-file)
                                                auto-file))))
                                     (--map (doom-module-path (car it) (cdr it)) doom-enabled-modules)))
                   (f-glob "autoload/*.el" doom-core-dir))))
      (when (f-exists-p generated-autoload-file)
        (f-delete generated-autoload-file)
        (message "Deleted old autoloads.el"))
      (dolist (file autoload-files)
        (update-file-autoloads file)
        (message "Scanned %s" (f-relative file doom-emacs-dir)))
      (with-current-buffer (get-file-buffer generated-autoload-file)
        (save-buffer)
        (eval-buffer))
      (message "Done!"))))

(defun doom/byte-compile (&optional comprehensive-p)
  "Byte (re)compile the important files in your emacs configuration (i.e.
init.el, core/*.el and modules/*/*/config.el) DOOM Emacs was designed to benefit
a lot from this.

If COMPREHENSIVE-P is non-nil, then compile modules/*/*/*.el (except for
packages.el files) -- this will likely take a long time."
  (interactive)
  (doom-reload)
  (let ((targets (append
                  (list (f-expand "init.el" doom-emacs-dir)
                        (f-expand "core.el" doom-core-dir))
                  (f-glob "core-*.el" doom-core-dir)
                  (-flatten
                   (--map (f--entries (doom-module-path (car it) (cdr it))
                                      (and (f-ext-p it "el")
                                           (or comprehensive-p
                                               (string= (f-base it) "config")
                                               (string-prefix-p "+" (f-base it))))
                                      t)
                          doom-enabled-modules))))
        (n 0)
        results)
    (mapc (lambda (file)
            (push (cons (f-relative file doom-emacs-dir)
                        (when (byte-recompile-file file nil 0)
                          (setq n (1+ n))
                          t))
                  results))
          targets)
    (when noninteractive
      (when targets (message "\n"))
      (message "Compiling %s files:\n%s" n
               (mapconcat (lambda (file) (concat "+ " (if (cdr file) "SUCCESS" "FAIL") ": " (car file)))
                          (reverse results) "\n")))))

(provide 'core-packages)
;;; core-packages.el ends here
