(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (progn
    (setq lua-indent-level tab-width)

    (when (file-exists-p "/Applications/love.app")
      (define-minor-mode love-mode
        "Buffer local minor mode for Love2D"
        :init-value nil
        :lighter " <3"
        :keymap (make-sparse-keymap)
        (narf/init-yas-mode 'love-mode))
      (add-hook! 'lua-mode-hook
                 (narf/set-build-command "open -a love.app '%s'" "main.lua")
                 (when (narf/project-has-files "main.lua")
                   (love-mode +1))))

    (after "company" (add-to-list 'company-dictionary-major-minor-modes 'love-mode))

    (add-hook! 'lua-mode-hook
               (narf|enable-tab-width-2)
               (setq lua-indent-level 2))))


(provide 'init-lua)
;;; init-lua.el ends here
