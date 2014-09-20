(provide 'init-webdev)

(use-package rainbow-mode
  :defer t
  :init
  (add-hook 'scss-mode 'rainbow-mode))

(use-package scss-mode
  :mode "\\.scss\\'"
  :config
  (progn
    (setq scss-compile-at-save nil)
    (add-hook 'scss-mode-hook 'ac-css-mode-setup)))

(use-package haml-mode :mode "\\.haml\\'")

;;; HTML/Markup
(use-package emmet-mode
  :defer t
  :config
  (setq emmet-move-cursor-between-quotes t)
  :init
  (progn
    (add-hook 'scss-mode-hook   'emmet-mode)
    (add-hook 'web-mode-hook    'emmet-mode)
    (add-hook 'html-mode-hook   'emmet-mode)
    (add-hook 'haml-mode-hook   'emmet-mode)
    (add-hook 'nxml-mode-hook   'emmet-mode)))

(use-package web-mode
  :mode (("\\.\\(p\\)?htm\\(l\\)?\\'" . web-mode)
         ("\\.tpl\\(\\.php\\)?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("wp-content/themes/.+/.+\\.php\\'" . web-mode))
  :config
  (progn
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))))

    (setq web-mode-markup-indent-offset  2
          web-mode-code-indent-offset    2
          web-mode-css-indent-offset     2
          web-mode-style-padding         2
          web-mode-script-padding        2
          web-mode-block-padding         2)

    (add-hook 'web-mode-hook 'enable-tab-width-2)))

(use-package php-mode
  :mode (("\\.php\\'" . php-mode)
         ("\\.inc\\'" . php-mode))
  :interpreter "php"
  :config
  (progn
    (add-hook! 'php-mode-hook (setq my-run-code-interpreter "php"))
    (setq php-template-compatibility nil)))

;;; Javascript
(use-package tern
  :commands tern-mode
  :config
  (progn
    (use-package tern-auto-complete
      :config (setq tern-ac-on-dot nil)))
  :init
  ;; replace auto-complete with tern-ac-complete only in js-mode
  (add-hook! 'js-mode-hook
             (tern-mode t)
             (tern-ac-setup)
             (setq my-run-code-interpreter "node")))

;; Jekyll support
(define-minor-mode jekyll-mode
  :init-value nil
  :lighter " :{")

(associate-minor-mode "[.-]jekyll/" jekyll-mode)
