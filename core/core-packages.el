;;; core-packages.el

;; Emacs is opinionated about package management. Unfortunately, so am I. So I
;; combined use-package, quelpa and package.el to manage my plugins.
;;
;; Why all the trouble? Because:
;; 1. Scriptability: I want my plugins managable from the command line (as well
;;    as an 'update-all-packages' command within emacs to update my plugins
;;    automatically, rather than through package.el's clunky interface).
;; 2. Flexibility: I want to install packages from sources other than ELPA
;;    repositories. Such as github or the Emacs wiki. Some plugins are out of
;;    date through official channels, have changed hands unofficially, or simply
;;    haven't been submitted to an ELPA repo yet.
;; 3. Stability: I don't want to be scared that every time I use my package
;;    management something may go wrong. This was the case with Cask, which I
;;    used previously. package.el and quelpa are much more stable.
;; 4. No external dependencies (e.g. Cask) for plugin management.

(defvar doom--init nil
  "Whether doom's package system has been initialized or not. It may not be if
you have byte-compiled your configuration (as intended).")

(defvar doom-packages (list (cons 'quelpa-use-package nil))
  "List of explicitly installed packages (not dependencies).")

(defvar doom-modules nil
  "List of enabled modules; each are cons cells whose car is the module's name
symbol and cdr is the submodule's name as a symbol.")

(defvar doom-auto-install-p nil
  "")

(defvar doom-dont-load-p nil
  "If non-nil, don't actually load modules, only keep track of them.")

(defvar doom--load-path (append (list doom-core-dir
                                      doom-modules-dir
                                      doom-local-dir)
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
  (unless doom-dont-load-p
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
  "Initialize installed packages (using package.el). This must be used on first
run, as it will prepare Emacs to auto-install all missing packages (otherwise
you'll get errors). If you byte compile core/core.el, calls to `package.el' are
avoided to speed up startup."
  (unless (or doom--init force-p)
    (setq load-path doom--load-path
          package-activated-list nil)
    (package-initialize)
    (unless package-archive-contents
      (package-read-all-archive-contents))
    (unless (package-installed-p 'quelpa-use-package)
      (package-refresh-contents)
      (package-install 'quelpa-use-package t)
      (setq doom-auto-install-p (not noninteractive)))
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

(defmacro package! (name &rest plist)
  "Uses `quelpa' and `use-package' to ensure PACKAGES are installed and
available. If `doom-auto-install-p' is nil, then strip out :ensure and :quelpa
properties, which is the case if you've byte-compiled DOOM Emacs.

It takes the same arguments as `use-package'.

Each element in PACKAGES can be a symbol or a list, whose car is the package
symbol and cdr is a plist. The plist accepts any argument `quelpa-use-package'
uses."
  (declare (indent defun))
  (let ((use-package-always-ensure doom-auto-install-p)
        (recipe (plist-get plist :quelpa)))
    ;; prepend NAME to quelpa recipe, if none is specified, to avoid local
    ;; MELPA lookups by quelpa.
    (when (and recipe (= 0 (mod (length recipe) 2)))
      (push name recipe)
      (plist-put plist :quelpa (append (list name) recipe)))
    (if doom-auto-install-p
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
  (load! :lang emacs-lisp)  loads modules/lang/emacs-lisp/{packages,config}.el

  ;; Note: requires that the calling module be loaded with `load!'
  (load! +local-module)     if called from ./config.el, loads ./+local-module.el"
  (let ((module-name (if (symbolp file-or-module-sym)
                         (symbol-name file-or-module-sym)
                       file-or-module-sym))
        submodule-name
        path file)
    (cond ((null submodule)
           (setq path (f-dirname load-file-name)
                 file (list module-name)))
          (t
           (when (string-prefix-p ":" module-name)
             (setq module-name (substring module-name 1)))
           (setq path (f-expand (concat module-name "/" (symbol-name submodule))
                                doom-modules-dir)
                 file (if doom-auto-install-p "packages.el" "config.el"))))
    (setq path (f-slash path)
          file (concat path file))
    (when (file-exists-p file)
      `(let ((__FILE__ ,file)
             (__DIR__  ,path))
         (load __FILE__ nil :noerror noninteractive noninteractive)))))

(defvar __DIR__ nil "The directory of the currently loaded file (with `load!')")
(defvar __FILE__ nil "The full path of the currently loaded file (with `load!')")


;;
;; Commands
;;

(defun doom-package-outdated-p (package)
  "Determine whether PACKAGE (a symbol) is outdated or not. Be sure to run
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
        (not (version-list-<= new-version cur-version))))))

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
  (let ((doom-auto-install-p t))
    (load (concat doom-emacs-dir "init.el"))))

(defun doom/packages-update ()
  "Update outdated packages. This includes quelpa itself, quelpa-installed
packages, and ELPA packages. This will delete old versions of packages as well."
  (interactive)
  (package-refresh-contents)
  (package-read-all-archive-contents)
  ;; first, upgrade quelpa + quelpa-installed packages
  (require 'quelpa)
  (let ((n 0)
        (err 0)
        (quelpa-upgrade-p t)
        quelpa-verbose)
    (when (quelpa-setup-p)
      (setq quelpa-cache (--filter (package-installed-p (car it)) quelpa-cache))
      (dolist (package quelpa-cache)
        (condition-case ex
            (let ((old-version (ignore-errors
                                 (package-desc-version
                                  (cadr (or (assq (car package) package-alist)
                                            (assq (car package) package--builtins))))))
                  new-version)
              (when (doom-package-outdated-p (car package))
                (setq n (1+ n))
                (let ((inhibit-message t))
                  (quelpa package))
                (setq new-version (package-desc-version
                                   (cadr (or (assq (car package) package-alist)
                                             (assq (car package) package--builtins)))))
                (when noninteractive
                  (message "Updating %s (%s -> %s) (quelpa)" (car package)
                           (mapconcat 'number-to-string old-version ".")
                           (mapconcat 'number-to-string new-version ".")))))
          ('error
           (setq err (1+ err))
           (message "ERROR (quelpa): %s" ex)))))
    ;; ...then update elpa packages
    (mapc (lambda (package)
            (when noninteractive (message "Updating %s (elpa)" package))
            (condition-case ex
                (let ((desc (cadr (assq package package-alist)))
                      (archive (cadr (assoc package package-archive-contents))))
                  (setq n (1+ n))
                  (package-install-from-archive archive)
                  (delete-directory (package-desc-dir desc) t))
              ('error
               (setq err (1+ err))
               (message "ERROR (elpa): %s" ex)))) ;; TODO real error string
          (-uniq (--filter (and (not (assq it quelpa-cache))
                                (doom-package-outdated-p it))
                           (package--find-non-dependencies))))
    (when noninteractive
      (message (if (= n 0)
                   "Everything is up-to-date"
                 "Updated %s packages") n)
      (when (> err 0)
        (message "There were %s errors" err)))))

(defun doom/packages-clean ()
  "Delete packages that are no longer used or referred to."
  (interactive)
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

(defun doom/byte-compile (&optional comprehensive-p)
  "Byte (re)compile the important files in your emacs configuration (init.el and
core/*.el). If COMPREHENSIVE-P is non-nil, also compile config.el files in
modules. DOOM Emacs was designed to benefit a lot from this."
  (interactive)
  (let (use-package-always-ensure
        file-name-handler-alist)
    (mapc 'byte-compile-file
          (append (list (f-expand "init.el" doom-emacs-dir)
                        (f-expand "core.el" doom-core-dir))
                  (reverse (f-glob "core-*.el" doom-core-dir))
                  (when comprehensive-p
                    (f-glob "*/*/config.el" doom-modules-dir))))))

(defun doom/refresh-autoloads ()
  "Refreshes the autoloads.el file, which tells Emacs where to find all the
autoloaded functions in the modules you use or among the core libraries.

Rerun this whenever you modify your init.el (or use `make autoloads` from the
command line)."
  (interactive)
  (let ((generated-autoload-file (concat doom-local-dir "autoloads.el"))
        (autoload-files
         (append
          (-flatten (mapcar (lambda (m)
                              (let* ((dir (f-expand (format "%s/%s"
                                                            (substring (symbol-name (car m)) 1)
                                                            (cdr m))
                                                    doom-modules-dir))
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
