;; -*- no-byte-compile: t; -*-
;;; ../core/test/core-projects.el

(require 'projectile)

;;
;; `doom-project-p'
(def-test! project-p
  :minor-mode projectile-mode
  (let ((buffer-file-name (expand-file-name "init.el" doom-emacs-dir))
        (default-directory doom-emacs-dir))
    (should (doom-project-p)))
  (let ((buffer-file-name (expand-file-name "test" "~"))
        (default-directory (expand-file-name "~")))
    (should-not (doom-project-p))))

;; `doom-project-root'
(def-test! project-root
  :minor-mode projectile-mode
  ;; Should resolve to project root
  (let ((buffer-file-name (expand-file-name "core.el" doom-core-dir))
        (default-directory doom-core-dir))
    (should (equal (doom-project-root) doom-emacs-dir)))
  ;; Should resolve to `default-directory' if not a project
  (let ((buffer-file-name (expand-file-name "test" "~"))
        (default-directory (expand-file-name "~")))
    (should (equal (doom-project-root) default-directory))))

;; `doom-project-expand'
(def-test! project-expand
  :minor-mode projectile-mode
  (let ((default-directory doom-core-dir))
    (should (equal (doom-project-expand "init.el")
                   (expand-file-name "init.el" (doom-project-root))))))

;; `project-file-exists-p!'
(def-test! project-has!
  :minor-mode projectile-mode
  (let ((default-directory doom-core-dir))
    ;; Resolve from project root
    (should (project-file-exists-p! "init.el"))
    ;; Chained file checks
    (should (project-file-exists-p! (and "init.el" "LICENSE")))
    (should (project-file-exists-p! (or "init.el" "does-not-exist")))
    (should (project-file-exists-p! (and "init.el" (or "LICENSE" "does-not-exist"))))
    ;; Should resolve relative paths from `default-directory'
    (should (project-file-exists-p! (and "core/core.el" "./init.el")))))
