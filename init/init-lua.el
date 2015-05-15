(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (progn
    (define-minor-mode love-mode
      "Buffer local minor mode for Love2D"
      :init-value nil
      :lighter " <3"
      :keymap (make-sparse-keymap)
      (my--init-yas-mode 'love-mode))
    (add-hook! 'lua-mode-hook
               (setq lua-indent-level tab-width)
               (set-build-command "open -a love.app '%s'" "main.lua")
               (when (project-has-files "main.lua")
                 (love-mode +1)))

    (after "company" (add-to-list 'company-dictionary-major-minor-modes 'love-mode))

    (add-hook 'lua-mode-hook 'enable-tab-width-2)))


(provide 'init-lua)
;;; init-lua.el ends here
