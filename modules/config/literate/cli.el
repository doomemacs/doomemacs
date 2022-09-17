;;; config/literate/cli.el -*- lexical-binding: t; -*-

(load! "autoload")

(defun +literate-add-installed-org-to-load-path-h ()
  "Use the straight-installed, not bundled Org."
  (let ((straight-org-build-dir
         (doom-path straight-base-dir "straight" straight-build-dir "org"))
        (straight-org-repo-dir
         (doom-path straight-base-dir "straight" "repos" "org")))
    (cond
     ((file-exists-p straight-org-build-dir)
      (add-to-list 'load-path straight-org-build-dir))
     ((file-exists-p straight-org-repo-dir)
      (add-to-list 'load-path straight-org-repo-dir)))))

;; Tangle the user's config.org before 'doom sync' runs
(add-hook 'doom-before-sync-hook #'+literate-tangle-h)
(add-hook 'doom-before-sync-hook #'+literate-add-installed-org-to-load-path-h)
