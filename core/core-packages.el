;;; core-packages.el

;; Emacs has always been opinionated about package management. Unfortunately, so
;; have I. I used to use Cask, but found it very unreliable, so the result is
;; this: a mini-package manager I've written by combining the functionality of
;; package.el, quelpa and use-package.
;;
;; Why all the trouble? Because I want to be able to call an
;; 'update-all-packages' function from a shell script and the like. I want to be
;; able to manage my Emacs' packages from the command line, because I have tools
;; in my dotfiles to help me auto-update everything with a one-liner.

(defvar doom-init nil
  "Whether doom's package system has been initialized or not. It may not be if
you have byte-compiled your configuration (as intended).")

(defvar doom-packages '(quelpa-use-package)
  "List of enabled packages.")

(defvar doom-modules nil
  "List of installed modules.")

(defvar doom-packages-auto-p (not noninteractive)
  "")

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

      use-package-always-ensure t
      use-package-always-defer t
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-dir (expand-file-name "quelpa" doom-packages-dir))


;;
;; Bootstrap
;;

(autoload 'use-package "use-package")

(defun doom-package-init (&optional force-p)
  "Initialize DOOM, its essential packages and package.el. This must be used on
first run. If you byte compile core/core.el, this file is avoided to speed up
startup."
  (when (or (not doom-init) force-p)
    (package-initialize)
    (unless (file-exists-p package-user-dir)
      (package-refresh-contents))
    (unless (package-installed-p 'quelpa-use-package)
      (package-install 'quelpa-use-package t))

    (require 'quelpa)
    (require 'use-package)
    (when doom-packages-auto-p
      (setq use-package-always-ensure t)
      (require 'quelpa-use-package)
      (quelpa-use-package-activate-advice))

    (add-to-list 'load-path doom-modules-dir)
    (add-to-list 'load-path doom-private-dir)
    (add-to-list 'load-path doom-core-dir)

    (setq doom-init t)))

(defmacro package! (name &rest rest)
  "Declare a package. Wraps around `use-package', and takes the same arguments
that it does."
  (declare (indent defun))
  (add-to-list 'doom-packages name)
  (unless doom-packages-auto-p
    (when (and (plist-member rest :ensure)
               (plist-get rest :ensure))
      (setq rest (plist-put rest :ensure nil)))
    (when (plist-member rest :quelpa)
      (use-package-plist-delete rest :quelpa)))
  (macroexpand-all `(use-package ,name ,@rest)))

(defmacro load-internal! (module package)

  )

(defmacro load! (plugin)

  )


;;
;; Commands
;;

(defun doom-package-outdated-p (package &optional inhibit-refresh)
  "Determine whether PACKAGE (a symbol) is outdated or not. If INHIBIT-REFRESH
is non-nil, don't run `package-refresh-contents' (which is slow. Use for batch
processing, and run `package-refresh-contents' once, beforehand)."
  (unless inhibit-refresh
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
  "Reload DOOM Emacs. This includes the `load-path'"
  (interactive)
  (setq load-path doom--load-path)
  (doom-package-init t)
  (when (called-interactively-p 'interactive)
    (message "Reloaded %s packages" (length package-alist))))

(defun doom/packages-install ()
  "Activate `doom-packages-auto-p' and evaluation your emacs configuration
again, in order to auto-install all packages that are missing."
  (interactive)
  (setq doom-packages-auto-p t)
  (doom-package-init t)
  (load (expand-file-name "init.el" doom-emacs-dir) nil nil t))

(defun doom/packages-update ()
  "Update all installed packages. This includes quelpa itself, quelpa-installed
packages, and ELPA packages."
  (interactive)
  (doom-package-init t)
  (quelpa-upgrade) ; upgrade quelpa + quelpa-installed packages
  (package-refresh-contents) ; ...then packages.el
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
  "Delete unused packages."
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

(defun doom/byte-compile ()
  "Byte (re)compile the important files in your emacs configuration. No more no
less."
  (interactive)
  (doom-package-init)
  (mapc 'byte-compile-file
        (append (list (expand-file-name "init.el" doom-emacs-dir)
                      (expand-file-name "core.el" doom-core-dir))
                (reverse
                 (file-expand-wildcards
                  (expand-file-name "core*.el" doom-core-dir)))
                (file-expand-wildcards
                 (expand-file-name "*/*/packages.el" doom-modules-dir)))))

(defun doom/refresh-autoloads (&optional inhibit-require)
  "Refreshes your emacs config's autoloads file. Use this if you modify an
autoload.el file in any module."
  (interactive)
  (let ((generated-autoload-file (concat doom-modules-dir "autoloads.el"))
        (interactive-p (called-interactively-p 'interactive))
        (autoload-files
         (file-expand-wildcards
          (expand-file-name "*/*/autoload.el" doom-modules-dir))))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file)
      (when interactive-p (message "Deleted old autoloads.el")))
    (dolist (file autoload-files)
      (update-file-autoloads file t)
      (when interactive-p
        (message "Detected: %s"
                 (file-relative-name (directory-file-name parent)
                                     doom-emacs-dir))))
    (when interactive-p (message "Done!"))
    (with-demoted-errors "WARNING: %s"
      (load "autoloads"))))

(provide 'core-packages)
;;; core-packages.el ends here
