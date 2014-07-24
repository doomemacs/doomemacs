
(add-hook 'lua-mode-hook
          (lambda()
              (evil-define-key 'normal lua-mode-map (kbd "s-r")
                (lambda() (interactive) (shell-command-on-region (point-min) (point-max) "lua")))
              (evil-define-key 'visual lua-mode-map (kbd "s-r")
                (lambda() (interactive) (shell-command-on-region (region-beginning) (region-end) "lua")))

              (define-key lua-mode-map (kbd "s-b")
                (lambda() (shell-command (concat "love " default-directory)))
            )))

;;
(provide 'env-lua-mode)
