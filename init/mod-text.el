(add-hook 'text-mode-hook 'my/enable-hard-wrap)

(use-package markdown-mode :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("/README\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook
            (lambda()
              ;; Restore native mac M-left/right functionality
              (local-unset-key (kbd "<M-left>"))
              (local-unset-key (kbd "<M-right>")))))


;;
(provide 'mod-text)
