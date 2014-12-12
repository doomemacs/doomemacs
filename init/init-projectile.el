(use-package projectile
  :pre-load
  (setq-default projectile-cache-file (concat my-tmp-dir "projectile.cache")
                projectile-known-projects-file (concat my-tmp-dir "projectile.projects")
                projectile-enable-caching t
                projectile-indexing-method 'alien)
  :init
  (projectile-global-mode +1)
  :config
  (progn
    (add-to-list 'projectile-globally-ignored-files "ido.last")
    (add-to-list 'projectile-globally-ignored-directories "assets")
    (add-to-list 'projectile-other-file-alist '("scss" "css"))
    (add-to-list 'projectile-other-file-alist '("css" "scss"))

    ;; For setting project-specific settings
    (defmacro my-project-settings (project-name &rest body)
      (declare (indent 1))
      `(progn
         (add-hook 'find-file-hook
                   (lambda ()
                     (when (string-match-p ,project-name (projectile-project-name))
                       ,@body)))))

    (after "perspective"
      (defvar persp-modestring-dividers '("" " |" ","))
      (use-package persp-projectile))))


(provide 'init-projectile)
;;; init-projectile.el ends here
