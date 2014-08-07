
(mapc 'my/install-package '(lua-mode))

(use-package lua-mode
    :mode "\\.lua\\'"
    :interpreter "lua"
    :init
    (add-hook 'lua-mode-hook
        (lambda()
          (nmap lua-mode-map (kbd "s-r")
                (lambda() (interactive) (shell-command-on-region (point-min) (point-max) "lua")))
          (vmap lua-mode-map (kbd "s-r")
                (lambda() (interactive) (shell-command-on-region (region-beginning) (region-end) "lua")))

          (define-key lua-mode-map (kbd "s-b")
            (lambda() (shell-command (concat "love " default-directory)))
            ))))

(use-package c++-mode :mode "\\.h\\'")

;;
(provide 'mod-gamedev)
