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

(defvar doom-init nil
  "Whether doom's package system has been initialized or not. It may not be if
you have byte-compiled your configuration (as intended).")

(defvar doom-packages '(quelpa-use-package)
  "List of enabled packages.")

(defvar doom-modules nil
  "List of installed modules.")

(defvar doom--load-path load-path
  "A backup of `load-path' before it was initialized.")

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
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-use-package-inhibit-loading-quelpa t
      quelpa-dir (expand-file-name "quelpa" doom-packages-dir))


;;
;; Library
;;

(defun doom-package-init (&optional force-p)
  "Initialize DOOM, its essential packages and package.el. This must be used on
first run. If you byte compile core/core.el, this file is avoided to speed up
startup. Returns `load-path'."
  (when (or (not doom-init) force-p)
    (package-initialize)
    (when (or (not (file-exists-p package-user-dir)) force-p)
      (package-refresh-contents))
    (unless (package-installed-p 'quelpa-use-package)
      (package-install 'quelpa-use-package t))

    (require 'quelpa-use-package)
    (setq use-package-always-ensure t)
    (quelpa-use-package-activate-advice)

    (add-to-list 'load-path doom-local-dir)
    (add-to-list 'load-path doom-modules-dir)
    (add-to-list 'load-path doom-core-dir)

    (setq doom-init t)))

(defmacro package! (name &rest rest)
  "Declare a package. Wraps around `use-package', and takes the same arguments
that it does. NOTE: Packages are deferred by default."
  (declare (indent defun))
  (add-to-list 'doom-packages name)
  ;; If `use-package-always-ensure' is nil, then remove any possibility of an
  ;; installation by package.el or quelpa.
  (unless use-package-always-ensure
    (when (and (plist-member rest :ensure)
               (plist-get rest :ensure))
      (setq rest (plist-put rest :ensure nil)))
    (when (plist-member rest :quelpa)
      (use-package-plist-delete rest :quelpa)))
  (macroexpand-all `(use-package ,name ,@rest)))

(defmacro load! (file-or-module-sym &optional submodule noerror)
  "Load a module from `doom-modules-dir'. Plays the same role as
`load-relative', but is specific to DOOM emacs modules and submodules.

Examples:
  (load! :lang emacs-lisp)  loads modules/lang/emacs-lisp/{packages,config}.el
  (load! +local-module)     if called from ./config.el, loads ./+local-module.el
                            Note: requires that config.el be loaded with `load!'"
  (let* ((module-name (symbol-name file-or-module-sym))
         (module (if (and submodule (string-prefix-p ":" module-name)) (substring module-name 1) module-name)))
    (if submodule
        (let ((path (concat doom-modules-dir module "/" (symbol-name submodule) "/")))
          (macroexp-progn
           (mapcar (lambda (file)
                     (when (file-exists-p (concat path file ".el"))
                       (doom--load path file (not doom-debug-mode))))
                   (append (list "packages")
                           (unless noninteractive (list "config"))))))
      (let ((path (concat (f-dirname load-file-name) "/")))
        (doom--load path module (not doom-debug-mode))))))

(defvar __DIR__ nil "The directory of the currently loaded file (with `load!')")
(defvar __FILE__ nil "The full path of the currently loaded file (with `load!')")

(defun doom--load (path file &optional noerror)
  `(let ((__FILE__ ,(concat path file (if noninteractive ".el")))
         (__DIR__ ,path))
     (load __FILE__ nil ,noerror noninteractive noninteractive)))

(defun doom-package-outdated-p (package &optional inhibit-refresh-p)
  "Determine whether PACKAGE (a symbol) is outdated or not. If INHIBIT-REFRESH-P
is non-nil, don't run `package-refresh-contents' (which is slow, but useful if
you intend to use this method for batch processing -- be sure to run
`package-refresh-contents' beforehand however)."
  (unless inhibit-refresh-p
    (package-refresh-contents))
  (when (and (package-installed-p package)
             (cadr (assq package package-archive-contents)))
    (let* ((newest-desc (cadr (assq package package-archive-contents)))
           (installed-desc (cadr (or (assq package package-alist)
                                     (assq package package--builtins))))
           (newest-version  (package-desc-version newest-desc))
           (installed-version (package-desc-version installed-desc)))
      (not (version-list-<= newest-version installed-version)))))

(defun doom/packages-reload ()
  "Reload `load-path' by scanning all packages. Run this if you ran make update
or make clean outside of Emacs."
  (interactive)
  (setq load-path doom--load-path)
  (doom-package-init t)
  (when (called-interactively-p 'interactive)
    (message "Reloaded %s packages" (length package-alist))))

(defun doom/packages-update ()
  "Update outdated packages. This includes quelpa itself, quelpa-installed
packages, and ELPA packages. This will delete old versions of packages as well."
  (interactive)
  (doom-package-init t)
  (quelpa-upgrade) ; upgrade quelpa + quelpa-installed packages
  (mapc (lambda (package)
          (condition-case ex
              (let ((desc (cadr (assq package package-alist))))
                (delete-directory (package-desc-dir desc) t)
                (package-install-from-archive (cadr (assoc package package-archive-contents))))
            ('error (message "ERROR: %s" ex)))) ;; TODO
        (-uniq (--filter (or (assq it quelpa-cache)
                             (doom-package-outdated-p it t))
                         (package--find-non-dependencies)))))

(defun doom/packages-clean ()
  "Delete packages that are no longer used or referred to."
  (interactive)
  (doom-package-init t)
  (let* ((package-selected-packages (-intersection (package--find-non-dependencies) doom-packages))
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
           (dolist (package packages-to-delete)
             (package-delete package t)
             (when (assq package quelpa-cache)
               (setq quelpa-cache (assq-delete-all package quelpa-cache)
                     quelpa-modified-p t)))
           (when quelpa-modified-p
             (quelpa-save-cache))))))

(defun doom/byte-compile (&optional comprehensive-p)
  "Byte (re)compile the important files in your emacs configuration. DOOM Emacs
was designed to benefit a lot from this. If COMPREHENSIVE-P is non-nil, compile
config.el and autoload.el files as well -- the performance benefit from this is
minor and may take a while.

No need to recompile any of these files so long as `auto-compile-mode' is on in
`emacs-lisp-mode', which, if you're using the provided emacs-lisp module, should
be the case."
  (interactive)
  (let (use-package-always-ensure)
    (doom-package-init)
    (mapc 'byte-compile-file
          (append (list (expand-file-name "init.el" doom-emacs-dir)
                        (expand-file-name "core.el" doom-core-dir))
                  (reverse
                   (f-glob "core-*.el" doom-core-dir))
                   (f-glob "*/*/packages.el" doom-modules-dir)
                  (when comprehensive-p
                    (f-glob "*/*/config.el" doom-modules-dir))
                  (when comprehensive-p
                    (f-glob "*/*/autoload.el" doom-modules-dir))))))

(defun doom/refresh-autoloads ()
  "Refreshes the autoloads.el file, which tells Emacs where to find all the
autoloaded functions in the modules you use or among the core libraries.

Rerun this whever you modify your init.el (or use `make autoloads` from the
command line)."
  (interactive)
  (let ((generated-autoload-file (concat doom-local-dir "autoloads.el"))
        (interactive-p (called-interactively-p 'interactive))
        (autoload-files
         (append (-filter 'file-exists-p
                          (mapcar (lambda (m)
                                    (f-expand
                                     (format "%s/%s/autoload.el" (substring (symbol-name (car m)) 1) (cdr m))
                                     doom-modules-dir))
                                  doom-modules))
                 (f-glob "autoload/*.el" doom-core-dir))))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file)
      (when interactive-p (message "Deleted old autoloads.el")))
    (dolist (file autoload-files)
      (update-file-autoloads file t)
      (unless interactive-p
        (message "Detected: %s" (f-relative file doom-emacs-dir))))
    (when interactive-p (message "Done!"))
    (with-demoted-errors "WARNING: %s"
      (load generated-autoload-file nil t))))

(provide 'core-packages)
;;; core-packages.el ends here
