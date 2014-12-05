(provide 'init-lua)

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (progn
    (define-minor-mode love-mode
      "Buffer local minor mode for Love2D"
      :init-value nil
      :lighter " <3"
      :keymap (make-sparse-keymap))
    (associate-minor-mode "[\\.-]love/.+\\.lua$" 'love-mode)

    (defun my--build-love ()
      (shell-command (format "open -a love.app %s" (my--project-root))))

    (add-hook! 'lua-mode-hook (setq my-run-code-interpreter "lua"))
    (add-hook! 'love-mode-hook (setq my-build-func 'my/build-love))))
