;; Package management bootstrap
(setq package-user-dir "~/.emacs.d/vendor/")
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(let ((default-directory my-elisp-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(package-initialize)

;; Run a body of code *after* a package is ready
(defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    (if (fboundp 'with-eval-after-load)
        `(with-eval-after-load ,feature ,@body)
        `(eval-after-load ,feature '(progn ,@body))))

;; Check if a package is installed; if load is t, load it too.
;; Works for packages bundled with emacs too!
(defun require-package (package)
  (message "=> require-package(%s)" package)
  (unless (require package nil 'noerror)
    (unless (package-installed-p package)
        (unless (assoc package package-archive-contents)
            (package-refresh-contents))
        (package-install package)))
  (require package))

;; List version of require-package
(defun require-packages (packages)
  (mapc 'require-package packages))

;; Associate an extension with a mode, and install the necessary
;; package(s) for it. Also look for a modules/env-*.el modefile for
;; extra configuration.
(defun associate-mode (mode ext &optional only-load-env)
  (let* ((mode-name (symbol-name mode))
         (env-mode-name (concat "env-" mode-name))
         (mode-path (expand-file-name (concat env-mode-name ".el") my-modules-dir)))

    (message "=> associate mode(%s)" mode-name)
    ;; (unless only-load-env (autoload mode mode-name))
    (unless only-load-env (require-package mode))
    (when (file-exists-p mode-path)
		(message "==> has extra-conf (%s)" env-mode-name)
		(require (intern env-mode-name))
		)
	)

  (if (typep ext 'list)
	  (dolist (e ext)
		(add-to-list 'auto-mode-alist `(,e . ,mode)))
	(add-to-list 'auto-mode-alist `(,ext . ,mode))))

;;
(provide 'core-packages)
