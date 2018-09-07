;; -*- no-byte-compile: t; -*-
;;; ui/doom-dashboard/test/test-doom-dashboard.el

(require 'core-projects)
(require 'projectile)
(require! :ui doom-dashboard)

(describe "ui/doom-dashboard"
  :var (default-directory projectile-enable-caching)
  (before-all (setq projectile-enable-caching nil))

  (before-each (projectile-mode +1))
  (after-each  (projectile-mode -1))

  (describe "get-pwd"
    :var (+doom-dashboard--last-cwd)
    (before-each
      (setq +doom-dashboard--last-cwd doom-core-dir
            default-directory doom-core-dir))
    (it "returns the current directory when policy is nil"
      (let (+doom-dashboard-pwd-policy)
        (expect (+doom-dashboard--get-pwd) :to-equal default-directory)))
    (it "returns a path if policy is a path"
      (let ((+doom-dashboard-pwd-policy "~"))
        (expect (+doom-dashboard--get-pwd) :to-equal (expand-file-name "~"))))
    (it "returns return value of policy as a function"
      (let ((+doom-dashboard-pwd-policy (lambda (x) "x")))
        (expect (+doom-dashboard--get-pwd) :to-equal "x")))
    (it "returns last cwd if policy is 'last"
      (let ((+doom-dashboard-pwd-policy 'last))
        (expect (+doom-dashboard--get-pwd) :to-equal doom-core-dir)))
    (it "returns last project if policy is 'last-project"
      (let ((+doom-dashboard-pwd-policy 'last-project))
        (expect (+doom-dashboard--get-pwd) :to-equal doom-emacs-dir))))

  (describe "dashboard-p"
    (it "changes the fallback buffer to the dashboard buffer"
      (expect (+doom-dashboard-p (doom-fallback-buffer))))))
