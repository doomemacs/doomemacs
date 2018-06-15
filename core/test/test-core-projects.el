;; -*- no-byte-compile: t; -*-
;;; ../core/test/test-core-projects.el

(require 'core-projects)

(describe "core/projects"
  (before-all (require 'projectile))
  (after-all  (unload-feature 'projectile t))

  (before-each (projectile-mode +1))
  (after-each (projectile-mode -1))

  (describe "project-p"
    (it "Should detect when in a valid project"
      (let ((buffer-file-name (expand-file-name "init.el" doom-emacs-dir))
            (default-directory doom-emacs-dir))
        (expect (doom-project-p))))
    (it "Should detect when not in a valid project"
      (let ((buffer-file-name (expand-file-name "test" "~"))
            (default-directory (expand-file-name "~")))
        (expect (doom-project-p) :to-be nil))))

  (describe "project-root"
    (it "should resolve to the project's root"
      (let ((buffer-file-name (expand-file-name "core.el" doom-core-dir))
            (default-directory doom-core-dir))
        (expect (doom-project-root) :to-equal doom-emacs-dir)))
    (it "should resolve to the `default-directory'"
      (let ((buffer-file-name (expand-file-name "test" "/"))
            (default-directory (expand-file-name "/")))
        (expect (doom-project-root) :to-equal default-directory))))

  (describe "project-expand"
    (it "expands to a path relative to the project root"
      (let ((default-directory doom-core-dir))
        (expect (doom-project-expand "init.el")
                :to-equal (expand-file-name "init.el" (doom-project-root))))))

  (describe "project-file-exists-p!"
    (let ((default-directory doom-core-dir))
      ;; Resolve from project root
      (expect (project-file-exists-p! "init.el"))
      ;; Chained file checks
      (expect (project-file-exists-p! (and "init.el" "LICENSE")))
      (expect (project-file-exists-p! (or "init.el" "does-not-exist")))
      (expect (project-file-exists-p! (and "init.el" (or "LICENSE" "does-not-exist")))))))
