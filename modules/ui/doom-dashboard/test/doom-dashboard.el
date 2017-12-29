;; -*- no-byte-compile: t; -*-
;;; ui/doom-dashboard/test/doom-dashboard.el

(require! :ui doom-dashboard)
(+doom-dashboard|init)

(defun -dashboard-test-cwd (spec file)
  (let ((+doom-dashboard-pwd-policy (car spec))
        +doom-dashboard--last-cwd
        projectile-enable-caching)
    (with-temp-buffer
      (setq-local buffer-file-name file)
      (setq-local default-directory (file-name-directory file)))
    (should +doom-dashboard--last-cwd)
    (+doom-dashboard-reload t)
    (should (equal (buffer-local-value 'default-directory (doom-fallback-buffer))
                   (cdr spec)))))

;;
(def-test! pwd-policy
  (let ((default-directory doom-emacs-dir)
        (+doom-dashboard--last-cwd doom-core-dir)
        projectile-enable-caching)
    (dolist (spec (list (cons 'last-project doom-emacs-dir)
                        (cons 'last doom-core-dir)
                        (cons (lambda (x) "x") "x")
                        (cons "~" (expand-file-name "~"))
                        (cons nil default-directory)))
      (let ((+doom-dashboard-pwd-policy (car spec)))
        (should (equal (+doom-dashboard--get-pwd) (cdr spec)))))))

(def-test! kill-buffer-query
  (let (+doom-dashboard--last-cwd)
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "core.el" doom-core-dir)
            default-directory doom-core-dir)
      (kill-buffer (current-buffer)))
    (should (equal +doom-dashboard--last-cwd doom-core-dir))))

(def-test! dashboard-reload-pwd-policy
  (dolist (spec (list (cons 'last-project doom-emacs-dir)
                      (cons 'last doom-core-dir)
                      (cons "~" (expand-file-name "~"))
                      (cons (lambda (x) "/tmp") (expand-file-name "/tmp"))))
    (-dashboard-test-cwd spec (expand-file-name "core.el" doom-core-dir))))


