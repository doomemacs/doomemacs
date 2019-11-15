;; -*- no-byte-compile: t; -*-
;;; core/test/test-core-projects.el

(describe "core/projects"
  :var (projectile-enable-caching)

  (require 'core-projects)
  (require 'projectile)

  (before-each
    (setq projectile-enable-caching nil)
    (projectile-mode +1))
  (after-each
    (projectile-mode -1))

  (describe "project-p"
    (it "Should detect when in a valid project"
      (expect (doom-project-p doom-emacs-dir)))
    (it "Should detect when not in a valid project"
      (expect (doom-project-p (expand-file-name "~")) :to-be nil)))

  (describe "project-root"
    (it "should resolve to the project's root"
      (expect (doom-project-root doom-core-dir) :to-equal-file doom-emacs-dir))
    (it "should return nil if not in a project"
      (expect (doom-project-root (expand-file-name "~")) :to-be nil)))

  (describe "project-expand"
    (it "expands to a path relative to the project root"
      (expect (doom-project-expand "init.el" doom-core-dir) :to-equal-file
              (expand-file-name "init.el" (doom-project-root doom-core-dir)))))

  (describe "project-file-exists-p!"
    (let ((default-directory doom-core-dir))
      ;; Resolve from project root
      (expect (project-file-exists-p! "init.el"))
      ;; Chained file checks
      (expect (project-file-exists-p! (and "init.el" "LICENSE")))
      (expect (project-file-exists-p! (or "init.el" "does-not-exist")))
      (expect (project-file-exists-p! (and "init.el" (or "LICENSE" "does-not-exist")))))))
