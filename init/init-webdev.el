(provide 'init-webdev)

(use-package rainbow-mode
  :defer t
  :init (add-hook 'scss-mode 'rainbow-mode))

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

    (setq web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-style-padding 2
          web-mode-script-padding 2
          web-mode-block-padding 2)
    ;; (setq web-mode-tag-auto-close-style 0)
    ;; (setq web-mode-enable-auto-opening t)
    ;; (setq web-mode-indent-style 1)

    (nvmap web-mode-map
          "]a" 'web-mode-attribute-next
          "]t" 'web-mode-tag-next
          "[t" 'web-mode-tag-previous
          "]T" 'web-mode-element-child
          "[T" 'web-mode-element-parent)

    (nmap web-mode-map
         "zf" 'web-mode-fold-or-unfold
         ",ct" 'web-mode-element-rename)

    (define-key web-mode-map (kbd "s-/") 'web-mode-comment-or-uncomment)

    (add-hook 'web-mode-hook
              (lambda()
                (setq indent-tabs-mode t)
                (setq tab-always-indent t)))
    (add-hook 'web-mode-hook 'enable-tab-width-2)
    ))

(use-package php-mode
  :mode (("\\.php\\'" . php-mode)
         ("\\.inc\\'" . php-mode))
  :interpreter "php"
  :config
  (progn
    (run-code-with "php" php-mode-map)
    (setq php-template-compatibility nil)))

;;; Javascript
(use-package tern
  :commands tern-mode
  :config
  (progn
    (run-code-with "node" js-mode-map)
    (use-package tern-auto-complete
      :config (setq tern-ac-on-dot nil)))
  :init
  ;; replace auto-complete with tern-ac-complete only in js-mode
  (add-hook 'js-mode-hook
    (lambda ()
      (tern-mode t)
      (tern-ac-setup)
      (imap js-mode-map [remap auto-complete] 'tern-ac-complete)
      )))

;; Jekyll support
(define-minor-mode jekyll-mode
  :init-value nil
  :lighter " :{"
  :keymap (make-sparse-keymap))

(associate-mode "[.-]jekyll/" jekyll-mode t)
