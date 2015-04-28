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

    (add-hook 'lua-mode-hook 'enable-tab-width-2)
    (add-hook! 'lua-mode-hook (setq lua-indent-level tab-width))

    (add-hook! 'love-mode-hook (setq my-build-command (format "open -a love.app %s" (my--project-root))))))


(provide 'init-lua)
;;; init-lua.el ends here
