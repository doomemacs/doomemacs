;; -*- no-byte-compile: t; -*-
;;; ui/doom-dashboard/test/doom-dashboard.el

(require! :ui doom-dashboard)

(defun -dashboard-test-pwd (spec file)
  (let ((kill-buffer-query-functions '(+doom-dashboard|reload-on-kill))
        (+doom-dashboard-pwd-policy (car spec))
        (fallback-buffer (doom-fallback-buffer))
        +doom-dashboard--last-cwd
        projectile-enable-caching)
    (with-temp-buffer
      (setq buffer-file-name file
            default-directory (file-name-directory file)
            doom-real-buffer-p t))
    (should +doom-dashboard--last-cwd)
    (+doom-dashboard-update-pwd)
    (should (equal (buffer-local-value 'default-directory fallback-buffer)
                   (cdr spec)))))

;;
(def-test! dashboard-p
  (let ((fallback-buffer (doom-fallback-buffer)))
    (should (equal (buffer-name fallback-buffer) +doom-dashboard-name))
    (should (+doom-dashboard-p fallback-buffer))))

(def-test! get-pwd
  :minor-mode projectile-mode
  (let ((default-directory doom-core-dir)
        (+doom-dashboard--last-cwd doom-core-dir)
        projectile-enable-caching)
    (dolist (spec (list (cons 'last-project doom-emacs-dir)
                        (cons 'last doom-core-dir)
                        (cons (lambda (x) "x") "x")
                        (cons "~" (expand-file-name "~"))
                        (cons nil default-directory)))
      (let ((+doom-dashboard-pwd-policy (car spec)))
        (should (equal (+doom-dashboard--get-pwd) (cdr spec)))))))

(def-test! pwd-policy
  :minor-mode projectile-mode
  (dolist (spec (list (cons 'last-project doom-emacs-dir)
                      (cons 'last doom-core-dir)
                      (cons "~" (expand-file-name "~/"))
                      (cons (lambda (x) "/tmp") "/tmp/")))
    (-dashboard-test-pwd spec (expand-file-name "core.el" doom-core-dir))))

;;
(def-test! inhibit-refresh :skip t)
(def-test! inhibit-functions :skip t)
