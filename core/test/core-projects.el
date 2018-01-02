;; -*- no-byte-compile: t; -*-
;;; ../core/test/core-projects.el

(require 'projectile)

;;
;; `doom-project-p'
(def-test! project-p
  :minor-mode projectile-mode
  (let ((default-directory doom-emacs-dir))
    (should (doom-project-p)))
  (let ((default-directory (expand-file-name "~")))
    (should-not (doom-project-p))))

;; `doom-project-p'
(def-test! project-root
  :minor-mode projectile-mode
  ;; Should resolve to project root
  (let ((default-directory doom-core-dir))
    (should (equal (doom-project-root) doom-emacs-dir)))
  ;; Should resolve to `default-directory' if not a project
  (let ((default-directory (expand-file-name "~")))
    (should (equal (doom-project-root) default-directory))))

;; `doom-project-expand'
(def-test! project-expand
  :minor-mode projectile-mode
  (let ((default-directory doom-core-dir))
    (should (equal (doom-project-expand "init.el")
                   (expand-file-name "init.el" (doom-project-root))))))

;; `doom-project-has!'
(def-test! project-has!
  :minor-mode projectile-mode
  (let ((default-directory doom-core-dir))
    ;; Resolve from project root
    (should (doom-project-has! "init.el"))
    ;; Chained file checks
    (should (doom-project-has! (and "init.el" "LICENSE")))
    (should (doom-project-has! (or "init.el" "does-not-exist")))
    (should (doom-project-has! (and "init.el" (or "LICENSE" "does-not-exist"))))
    ;; Should resolve relative paths from `default-directory'
    (should (doom-project-has! (and "./core.el" "../init.el")))))
