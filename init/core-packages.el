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
(defun require-package (package &optional dont_load)
  (unless (require package nil 'noerror)
    (init-package package dont_load)))

;; List version of require-package
(defun require-packages (packages &optional dont_load)
  (dolist (pkg packages) (require-package pkg dont_load)))

;; Install the package if it isn't already, and require it, unless
;; told others.
(defun init-package (package &optional dont_load)
    (unless (package-installed-p package)
        (unless (assoc package package-archive-contents)
            (package-refresh-contents))
        (package-install package))
    (if (not dont_load) (require package)))

;; Associate an extension with a mode, and install the necessary
;; package for it.
;;
;; TODO: Rewrite this
(defun associate-mode (mode ext &optional env-pkg)
  (let* ((mode_name (symbol-name mode))
         (env_mode_name (major-mode-module-name))
         (mode_path (major-mode-module-path)))

    (condition-case nil (init-package mode t) (error nil))

    (autoload mode mode_name)
    (if env-pkg 
      (require-package env-pkg)
      (if (file-exists-p mode_path) (require-package (intern env_mode_name)))
      ))

  (if (typep ext 'list)
    (dolist (e ext)
      (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" e) . ,mode)))
    (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" ext) . ,mode))))

;;
(provide 'core-packages)
