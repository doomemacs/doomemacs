(provide 'init-love)

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (run-code-with "lua" lua-mode-map)
  :init
  (progn
    (define-minor-mode love-mode
      "Buffer local minor mode for Love2D"
      :init-value nil
      :lighter " <3"
      :keymap (make-sparse-keymap) ; defines love-mode-map
      :group lua)

    ;; (add-to-list 'auto-minor-mode-alist '("[.-]love/.+\\.lua\\'" . love-mode))
    (associate-mode "[.-]love/.+\\.lua\\'" love-mode t)

    (nmap love-mode-map (kbd "s-b") ",b")
    (nmap love-mode-map (kbd ",b")
          `(lambda()
             (interactive)
             (let ((root (if (projectile-project-p) (projectile-project-root) default-directory)))
               (shell-command (concat "open -a love.app " (projectile-project-root))))))))
