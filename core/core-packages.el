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

(defvar doom-packages '((quelpa-use-package))
  "List of packages that have been explicitly installed (not dependencies) with
`package!'. Each element is a list whose car is the package symbol, and cdr is
its quelpa recipe, if available.")

(defvar doom-modules nil
  "List of enabled modules; each element is a cons cell (MODULE . SUBMODULE),
where MODULE is the module's property symbol, e.g. :lang, and SUBMODULE is the
submodule symbol, e.g. 'evil.")

(defvar doom--init nil
  "Non-nil if doom's package system has been initialized or not. It may not be
if you have byte-compiled your configuration (as intended).")

(defvar doom--auto-install-p nil
  "If non-nil, install missing packages. Otherwise, strip :ensure and :quelpa
from `package!' calls.")

(defvar doom--dont-load-p nil
  "If non-nil, don't actually load modules, only keep track of them.")

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
      use-package-debug doom-debug-mode
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
             (setq doom-modules (append doom-modules (list (cons mode p))))))))
  (unless doom--dont-load-p
    `(let (file-name-handler-alist)
       ,@(mapcar (lambda (pkg) (macroexpand `(load! ,(car pkg) ,(cdr pkg))))
                 doom-modules)

       (unless noninteractive
         (when (display-graphic-p)
           (require 'server)
           (unless (server-running-p)
             (server-start)))

         ;; Prevent any auto-displayed text + benchmarking
         (advice-add 'display-startup-echo-area-message :override 'ignore)
         (message "Loaded %s packages in %s"
                  (length doom-packages)
                  (emacs-init-time))))))

(defun doom-initialize (&optional force-p)
  "Initialize installed packages (using package.el). On first run it will
prepare Emacs to auto-install all missing packages. If you byte compile
core/core.el, calls to `package.el' are avoided to speed up startup."
  (unless (or doom--init force-p)
    (setq load-path doom--load-path
          package-activated-list nil)
    (package-initialize)
    (unless (package-installed-p 'quelpa-use-package)
      (package-refresh-contents)
      (package-install 'quelpa-use-package t)
      (setq doom--auto-install-p (not noninteractive)))
    (unless (featurep 'quelpa-use-package)
      (require 'quelpa-use-package)
      (quelpa-use-package-activate-advice)
      (setq use-package-expand-minimally t)
      ;; Move :ensure to after conditional properties
      (delq :ensure use-package-keywords)
      (push :ensure (cdr (memq :unless use-package-keywords))))
    (setq doom--init t)))


;;
;; Macros
;;

(defvar doom--packages nil
  "List of packages explicitly installed during this session.")

(defalias 'use-package! 'use-package
  "To adhere to the naming conventions of DOOM emacs.")

(defmacro package! (name &rest plist)
  "Wraps around `use-package' (with `quelpa-use-package') and takes the same
arguments. Ensures the package named NAME is installed and available. If
`doom--auto-install-p' is nil, then strip out :ensure and :quelpa properties,
which is the case if you've byte-compiled DOOM Emacs."
  (declare (indent defun))
  (let ((use-package-always-ensure doom--auto-install-p)
        (recipe (plist-get plist :quelpa)))
    ;; prepend NAME to quelpa recipe, if none is specified, to avoid local
    ;; MELPA lookups by quelpa.
    (when (and recipe (= 0 (mod (length recipe) 2)))
      (push name recipe)
      (plist-put plist :quelpa (append (list name) recipe)))
    (if doom--auto-install-p
        (unless (package-installed-p name)
          (add-to-list 'doom--packages name))
      (setq plist (use-package-plist-delete plist :ensure))
      (setq plist (use-package-plist-delete plist :quelpa)))
    `(progn
       (add-to-list 'doom-packages '(,name ,@recipe))
       ,(macroexpand-all `(use-package ,name ,@plist)))))

(defmacro load! (file-or-module-sym &optional submodule)
  "Load a module from `doom-modules-dir'. Plays the same role as
`load-relative', but is specific to DOOM emacs modules and submodules.

Examples:
(load! :lang emacs-lisp)

  Loads modules/lang/emacs-lisp/(packages|config).el; package.el if
  `doom--auto-install-p' is non-nil, config.el otherwise.

(load! +local-module)

  NOTE: Requires that the calling module be loaded with `load!'.
  If called from ./config.el, loads ./+local-module.el"
  (let (path file)
    (cond ((null submodule)
           (setq path (f-dirname load-file-name)
                 file (list (if (symbolp file-or-module-sym)
                                (symbol-name file-or-module-sym)
                              file-or-module-sym))))
          (t
           (setq path (f-slash (doom-module-path file-or-module-sym submodule))
                 file (if doom--auto-install-p "packages.el" "config.el"))))
    (setq path (f-slash path)
          file (concat path file))
    (when (f-exists-p file)
      `(let ((__FILE__ ,file)
             (__DIR__  ,path))
         (load ,(if noninteractive file (f-no-ext file))
               nil ,(not doom-debug-mode) noninteractive)))))

(defvar __DIR__ nil "The directory of the currently loaded file (with `load!')")
(defvar __FILE__ nil "The full path of the currently loaded file (with `load!')")

(defun doom-module-path (module submodule &optional file)
  (f-expand (concat (substring (symbol-name module) 1) "/" (symbol-name submodule) "/" file)
            doom-modules-dir))


;;
;; Commands
;;

(defun doom-package-outdated-p (package)
  "Determine whether PACKAGE (a symbol) is outdated or not. If outdated, returns
a cons cell, whose car is the current version string of PACKAGE (a symbol), and
whose cdr is the latest version of the package. Be sure to run
`package-refresh-contents' beforehand, or the return value could be out of
date."
  (let ((pkg (assq package doom-packages)))
    (when (and pkg (package-installed-p package))
      (let* ((pkg-recipe (cdr pkg))
             (cur-desc (cadr (or (assq package package-alist)
                                 (assq package package--builtins))))
             (cur-version (package-desc-version cur-desc))
             (inhibit-message t)
             new-version)
        (setq new-version
              (if pkg-recipe
                  (let ((ver (quelpa-checkout
                              pkg-recipe
                              (f-expand (symbol-name package) quelpa-build-dir))))
                    (or (and ver (version-to-list ver)) cur-version))
                (package-desc-version (cadr (assq package package-archive-contents)))))
        (unless (version-list-<= new-version cur-version)
          (cons cur-version new-version))))))

(defun doom/packages-reload ()
  "Reload `load-path' by scanning all packages. Run this if you ran make update
or make clean outside of Emacs."
  (interactive)
  (doom-initialize t)
  (when (called-interactively-p 'interactive)
    (message "Reloaded %s packages" (length package-alist))))

(defun doom/packages-install ()
  "Install missing packages."
  (interactive)
  (declare (interactive-only t))
  (doom-initialize t)
  (let ((doom--auto-install-p t))
    (load (concat doom-emacs-dir "init.el") nil nil t)))

(defun doom/packages-update ()
  "Update outdated packages. This includes quelpa-installed packages and ELPA
packages. This will delete old versions of packages as well."
  (interactive)
  (message "Refreshing packages...")
  (package-initialize)
  (package-refresh-contents)
  (if (not package-alist)
      (message "No packages are installed")
    (require 'quelpa)
    (when (quelpa-setup-p)
      (setq quelpa-cache (--filter (package-installed-p (car it)) quelpa-cache)))
    (let* ((err 0)
           (quelpa-packages (-map 'car quelpa-cache))
           (elpa-packages (-difference (package--find-non-dependencies) quelpa-packages))
           outdated-packages)
      (dolist (pkg (append quelpa-packages elpa-packages))
        (-when-let (ver (doom-package-outdated-p pkg))
          (push (list pkg ver) outdated-packages)))
      (cond ((not outdated-packages)
             (message "Everything is up-to-date"))
            ((not (y-or-n-p
                   (format "%s packages will be updated:\n%s\n\nProceed?"
                           (length outdated-packages)
                           (mapconcat (lambda (pkg) (format "%s: %s -> %s"
                                                       (car pkg)
                                                       (car (cdr pkg))
                                                       (cdr (cdr pkg))))
                                      (--sort (string-lessp (symbol-name (car it))
                                                            (symbol-name (car other)))
                                              outdated-packages) ", "))))
             (message "Aborted"))
            (t
             (dolist (pkg outdated-packages)
               (condition-case ex
                   (cond ((assq pkg quelpa-outdated-packages)
                          (let ((inhibit-message t))
                            (quelpa package)))
                         ((memq pkg elpa-outdated-packages)
                          (let ((desc (cadr (assq pkg package-alist)))
                                (archive (cadr (assoc pkg package-archive-contents))))
                            (package-install-from-archive archive)
                            (delete-directory (package-desc-dir desc) t)))
                         (t (error "Not a valid package")))
                 ('error
                  (setq err (1+ err))
                  (message "ERROR (%s): %s" pkg ex))))))
      (if (> err 0)
          (message "Done, but with %s errors" err)
        (message "Done")))))

(defun doom/packages-clean ()
  "Delete packages that are no longer used or depended on."
  (interactive)
  (package-initialize)
  (let* ((package-selected-packages (-intersection (package--find-non-dependencies)
                                                   (mapcar 'car doom-packages)))
         (packages-to-delete (package--removable-packages))
         quelpa-modified-p)
    (cond ((not package-selected-packages)
           (message "No packages installed!"))
          ((not packages-to-delete)
           (message "No unused packages to remove."))
          ((not (y-or-n-p
                 (format "%s packages will be deleted:\n%s\n\nProceed?"
                         (length packages-to-delete)
                         (mapconcat 'symbol-name (reverse packages-to-delete) ", "))))
           (message "Aborted."))
          (t
           (require 'quelpa)
           (quelpa-setup-p)
           (dolist (p packages-to-delete)
             (package-delete (cadr (assq p package-alist)) t)
             (when (and quelpa-cache (assq p quelpa-cache))
               (setq quelpa-cache (assq-delete-all p quelpa-cache)
                     quelpa-modified-p t)))
           (when quelpa-modified-p
             (quelpa-save-cache))))))

(defun doom/byte-compile ()
  "Byte (re)compile the important files in your emacs configuration (i.e.
init.el, core/*.el and modules/*/*/config.el) DOOM Emacs was designed to benefit
a lot from this."
  (interactive)
  (doom-initialize)
  (let ((targets (append (list (f-expand "init.el" doom-emacs-dir)
                               (f-expand "core.el" doom-core-dir))
                         (reverse (f-glob "core-*.el" doom-core-dir))
                         (-filter 'f-exists-p
                                  (--map (doom-module-path (car it) (cdr it) "config.el")
                                         doom-modules))))
        use-package-always-ensure
        file-name-handler-alist)
    (mapc 'byte-compile-file targets)
    (when noninteractive
      (when targets (message "\n"))
      (message "Compiled %s files:\n%s" (length targets)
               (mapconcat (lambda (file) (concat "+ " (f-relative file doom-emacs-dir)))
                          targets "\n")))))

(defun doom/refresh-autoloads ()
  "Refreshes the autoloads.el file, which tells Emacs where to find all the
autoloaded functions in the modules you use or among the core libraries.

Rerun this whenever you modify your init.el (or use `make autoloads` from the
command line)."
  (interactive)
  ;; Reload modules (don't load anything)
  (setq doom-modules nil)
  (let ((doom--dont-load-p t)
        (noninteractive t))
    (load (concat doom-emacs-dir "init.el") nil :nomessage t))

  (let ((generated-autoload-file (concat doom-local-dir "autoloads.el"))
        (autoload-files
         (append
          (-flatten (mapcar (lambda (m)
                              (let* ((dir (doom-module-path (car m) (cdr m)))
                                     (auto-dir (f-expand "autoload" dir))
                                     (auto-file (f-expand "autoload.el" dir)))
                                (cond ((f-directory-p auto-dir)
                                       (f-glob "*.el" auto-dir))
                                      ((f-exists-p auto-file)
                                       auto-file))))
                            doom-modules))
          (f-glob "autoload/*.el" doom-core-dir))))
    (when (f-exists-p generated-autoload-file)
      (delete-file generated-autoload-file)
      (when noninteractive (message "Deleted old autoloads.el")))
    (dolist (file autoload-files)
      (update-file-autoloads file)
      (when noninteractive
        (message "Detected: %s" (f-relative file doom-emacs-dir))))
    (with-current-buffer (get-file-buffer generated-autoload-file)
      (save-buffer))
    (when noninteractive (message "Done!"))
    (with-demoted-errors "WARNING: %s"
      (load generated-autoload-file nil t))))

(provide 'core-packages)
;;; core-packages.el ends here
