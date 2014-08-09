(use-package rainbow-mode :ensure t
  :defer t
  :init (add-hook 'scss-mode 'rainbow-mode))

(use-package scss-mode :ensure t
  :mode "\\.scss\\'"
  :config
  (add-hook 'scss-mode-hook (lambda() (add-to-list 'ac-sources 'ac-css-mode-setup))))

(use-package haml-mode :ensure t :mode "\\.haml\\'")
(use-package web-mode :ensure t
  :mode (("\\.\\(p\\)?htm\\(l\\)?\\'" . web-mode)
         ("\\.tpl\\(\\.php\\)?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)))

(use-package php-mode :ensure t
  :mode (("\\.php\\'" . php-mode)
         ("\\.inc\\'" . php-mode))
  :interpreter "php"
  :config
  (progn
    (my/setup-run-code php-mode-map "php")
    (setq php-template-compatibility nil)))

(use-package js-mode
  :mode "\\.js\\'"
  :interpreter "node")

(use-package tern :ensure t
  :commands tern-mode
  :config
  (progn
    (my/setup-run-code js-mode-map "node")
    (use-package tern-auto-complete :ensure t
      :config (setq tern-ac-on-dot nil)))
  :init
  ;; replace auto-complete with tern-ac-complete only in js-mode
  (add-hook 'js-mode-hook
    (lambda ()
      (tern-mode t)
      (imap js-mode-map (kbd "C-SPC") 'tern-ac-complete)
      (tern-ac-setup))))

(use-package emmet-mode :ensure t
  :defer t
  :config
  (imap 'emmet-mode-map (kbd "s-e") 'emmet-expand-line)
  :init
  (progn
    (add-hook 'scss-mode-hook   'emmet-mode)
    (add-hook 'web-mode-hook    'emmet-mode)
    (add-hook 'nxml-mode-hook   'emmet-mode)))

;;
(provide 'mod-webdev)
