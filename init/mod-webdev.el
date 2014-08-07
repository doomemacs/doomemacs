(mapc 'my/install-package
      '(emmet-mode
        scss-mode
        web-mode
        haml-mode
        tern
        tern-auto-complete
        php-mode
        rainbow-mode            ; highlight color codes
        ))

(use-package rainbow-mode
  :defer t
  :init (add-hook 'scss-mode 'rainbow-mode))

(use-package scss-mode :mode "\\.scss\\'")
(use-package haml-mode :mode "\\.haml\\'")
(use-package web-mode
    :mode (("\\.\\(p\\)?htm\\(l\\)?\\'" . web-mode)
           ("\\.tpl\\(\\.php\\)?\\'" . web-mode)
           ("\\.erb\\'" . web-mode)))

(use-package php-mode
  :mode "\\.php\\'"
  :interpreter "php"
  :config
  (setq php-template-compatibility nil))

(use-package tern
    :commands (tern-mode tern-ac-complete tern-ac-setup)
    :config
    (progn (use-package tern-auto-complete)
           (setq tern-ac-on-dot nil))

    :init
      ;; replace auto-complete with tern-ac-complete only in js-mode
      (add-hook 'js-mode-hook
        (lambda ()
          (imap ac-mode-map (kbd "C-SPC") 'tern-ac-complete)
          (tern-mode t)
          (tern-ac-setup))))

(use-package emmet-mode
    :defer t
    :config
    (progn
      (imap 'emmet-mode-map (kbd "s-e") 'emmet-expand-line)

      (add-hook 'scss-mode-hook   'emmet-mode)
      (add-hook 'web-mode-hook    'emmet-mode)
      (add-hook 'nxml-mode-hook   'emmet-mode))
    )

;;
(provide 'mod-webdev)
