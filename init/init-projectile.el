(use-package projectile
  :init (setq-default projectile-enable-caching t)
  :config
  (progn
    (projectile-global-mode +1)

    (setq projectile-sort-order 'recentf
          projectile-cache-file (concat my-tmp-dir "projectile.cache")
          projectile-known-projects-file (concat my-tmp-dir "projectile.projects")
          projectile-indexing-method 'alien)

    (add-to-list 'projectile-globally-ignored-files "ido.last")
    (add-to-list 'projectile-globally-ignored-directories "assets")
    (add-to-list 'projectile-other-file-alist '("scss" "css"))
    (add-to-list 'projectile-other-file-alist '("css" "scss"))))


(provide 'init-projectile)
;;; init-projectile.el ends here
