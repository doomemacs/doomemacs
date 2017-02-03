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

(defvar doom-protected-packages '(quelpa-use-package f s dash)
  "A list of packages that shouldn't be deleted.")

(defvar doom-installed-packages nil
  "A list of packages that were installed during the current session.")

(defvar doom--init nil
  "Non-nil if doom's package system has been initialized or not. It may not be
if you have byte-compiled your configuration (as intended).")

(defvar doom--auto-install-p nil
  "If non-nil, install missing packages. Otherwise, strip :ensure and :quelpa
from `package!' calls.")

(defvar doom--prefer-el-p (or noninteractive doom--auto-install-p)
  "If non-nil, load uncompiled .el config files.")

(defvar doom--load-path (append (list doom-core-dir
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
      quelpa-use-package-inhibit-loading-quelpa t
      quelpa-dir (expand-file-name "quelpa" doom-packages-dir)
      ;; ssh, no tears. Only compiling.
      byte-compile-warnings
      '(unresolved callargs obsolete noruntime cl-functions make-local constants suspicious))


;;
;; Bootstrap function
;;

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
  `(unless noninteractive
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
                (- (length load-path) (length doom--load-path))
                (emacs-init-time)))))

(defun doom-initialize (&optional force-p)
  "Initialize installed packages (using package.el) and ensure the core packages
are installed. If you byte compile core/core.el, calls to `package.el' are
avoided to speed up startup."
  (unless (or doom--init force-p)
    (setq load-path doom--load-path
          package-activated-list nil)
    (package-initialize)
    (unless (and (file-exists-p doom-packages-dir)
                 (require 'quelpa-use-package nil t))
      (package-refresh-contents)
      ;; Ensure core packages are installed
      (mapc 'package-install '(quelpa-use-package dash f s)))
    (unless (require 'quelpa-use-package nil t)
      (delete-directory doom-packages-dir t)
      (error "There was an error initializing DOOM. Try running it again"))
    (quelpa-use-package-activate-advice)
    ;; Move :ensure to after conditional properties
    (delq :ensure use-package-keywords)
    (push :ensure (cdr (memq :unless use-package-keywords)))
    (setq doom--init t)))

(defun doom-reload-modules ()
  "Reload `doom-modules'."
  (setq doom-modules nil)
  (let ((doom--prefer-el-p t)
        (noninteractive t))
    (load (concat doom-emacs-dir "init.el") nil :nomessage :nosuffix)))

(defun doom-reload-packages (&optional install-p)
  "Reload `doom-packages'."
  (doom-initialize)
  (doom-reload-modules)
  (let ((before-packages-n (length package-alist))
        (doom--auto-install-p (and install-p t))
        (doom--prefer-el-p t)
        (after-packages-n 0)
        noninteractive)
    (when doom--auto-install-p
      (package-refresh-contents))
    (load (f-expand "core.el" doom-core-dir) nil (not doom-debug-mode) :nosuffix)
    (mapc (lambda (pkg)
            (let ((path (f-expand "packages.el" (doom-module-path (car pkg) (cdr pkg)))))
              (when (f-exists-p path)
                (load path nil (not doom-debug-mode) :nosuffix))))
          doom-enabled-modules)
    (- (length package-alist) before-packages-n)))


;;
;; Macros
;;

(defvar __DIR__ nil  "The directory of the currently loaded file (set by `load!')")
(defvar __FILE__ nil "The full path of the currently loaded file (set by `load!')")

(defmacro use-package! (name &rest plist)
  "A `use-package' wrapper, to adhere to the naming conventions of DOOM emacs
and let-bind `package-name' for the containing forms. Note that packages are
deferred by default."
  `(let ((package-name ',name))
     (use-package ,name ,@plist)))

(defmacro package! (name &rest plist)
  "Wraps around `use-package' (with `quelpa-use-package') and takes the same
arguments. Ensures the package named NAME is installed and available. If
`doom--auto-install-p' is nil, then strip out :ensure and :quelpa properties,
which is the case if you've byte-compiled DOOM Emacs.

Note that packages are deferred by default."
  (declare (indent defun))
  (let ((use-package-always-ensure doom--auto-install-p)
        (recipe (plist-get plist :quelpa)))
    ;; prepend NAME to quelpa recipe, if none is specified, to avoid local
    ;; MELPA lookups by quelpa.
    (when (and recipe (= 0 (mod (length recipe) 2)))
      (push name recipe)
      (setq plist (plist-put plist :quelpa recipe)))
    (if doom--auto-install-p
        (unless (package-installed-p name)
          (pushnew name doom-installed-packages))
      (setq plist (use-package-plist-delete plist :ensure))
      (setq plist (use-package-plist-delete plist :quelpa)))
    ;; (package--save-selected-packages (cons name package-selected-packages))
    (pushnew name doom-packages)
    (macroexpand `(use-package ,name ,@plist))))

(defmacro load! (file-or-module-sym &optional submodule file)
  "Load a module from `doom-modules-dir'. Plays the same role as
`load-relative', but is specific to DOOM emacs modules and submodules. If
`doom--prefer-el-p' is non-nil, prefer the un-compiled elisp file.

Examples:
 (load! :lang emacs-lisp)

  Loads modules/lang/emacs-lisp/FILE.el (defaults to config.el).

 (load! +local-module)

  Loads +local-module.el relative to `__DIR__' or `doom-core-dir'."
  (let (path file)
    (cond ((null submodule)
           (setq path (or __DIR__ doom-core-dir)
                 file (concat (if (symbolp file-or-module-sym)
                                  (symbol-name file-or-module-sym)
                                file-or-module-sym)
                              ".el")))
          (t
           (setq path (doom-module-path file-or-module-sym submodule)
                 file (or file "config.el"))))
    (setq path (f-slash path)
          file (concat path file))
    `(let ((__FILE__ ,file)
           (__DIR__  ,path))
       (load (if doom--prefer-el-p ,file ,(f-no-ext file))
             nil (not doom-debug-mode) doom--prefer-el-p))))

(defun doom-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/. Will append FILE if non-nil."
  (setq module
        (cond ((keywordp module) (substring (symbol-name module) 1))
              ((symbolp module)  (symbol-name module))
              ((stringp module) module)
              (t (error "Not a valid module name: %s" module))))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (f-expand (concat module "/" submodule "/" file)
            doom-modules-dir))


;;
;; Defuns
;;

(defun doom/refresh-autoloads ()
  "Refreshes the autoloads.el file, which tells Emacs where to find all the
autoloaded functions in the modules you use or among the core libraries.

Rerun this whenever you modify your init.el (or use `make autoloads` from the
command line)."
  (interactive)
  (unless doom--init
    (doom-reload-modules))
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
      (message "Detected: %s" (f-relative file doom-emacs-dir)))
    (with-current-buffer (get-file-buffer generated-autoload-file)
      (save-buffer))
    (load generated-autoload-file nil t t)
    (message "Done!")))

(defun doom/byte-compile ()
  "Byte (re)compile the important files in your emacs configuration (i.e.
init.el, core/*.el and modules/*/*/config.el) DOOM Emacs was designed to benefit
a lot from this."
  (interactive)
  (doom-initialize)
  (doom-reload-modules)
  (let ((targets (append (list (f-expand "init.el" doom-emacs-dir)
                               (f-expand "core.el" doom-core-dir))
                         (reverse (f-glob "core-*.el" doom-core-dir))
                         (-filter 'f-exists-p
                                  (--map (doom-module-path (car it) (cdr it) "config.el")
                                         doom-enabled-modules))))
        (n 0)
        results
        use-package-always-ensure
        file-name-handler-alist)
    (mapc (lambda (file)
            (push (cons (f-relative file doom-emacs-dir)
                        (when (byte-compile-file file)
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
