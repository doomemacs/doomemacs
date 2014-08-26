(use-package rainbow-mode :ensure t
  :defer t
  :init (add-hook 'scss-mode 'rainbow-mode))

(use-package scss-mode :ensure t
  :mode "\\.scss\\'"
  :config
  (progn
    (setq scss-compile-at-save nil)
    (add-hook 'scss-mode-hook 'ac-css-mode-setup)))

(use-package haml-mode :ensure t :mode "\\.haml\\'")

(use-package php-mode :ensure t
  :mode (("\\.php\\'" . php-mode)
         ("\\.inc\\'" . php-mode))
  :interpreter "php"
  :config
  (progn
    (my/setup-run-code php-mode-map "php")
    (setq php-template-compatibility nil)))

(use-package web-mode :ensure t
  :mode (("\\.\\(p\\)?htm\\(l\\)?\\'" . web-mode)
         ("\\.tpl\\(\\.php\\)?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("wp-content/themes/.+/.+\\.php\\'" . web-mode))
  :config
  (progn
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))))

    (setq web-mode-markup-indent-offset 4
          web-mode-css-indent-offset 4
          web-mode-code-indent-offset 4)

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

    (add-hook 'web-mode-hook 'jekyll-mode-maybe)))

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
      (tern-ac-setup)
      (imap js-mode-map (kbd "C-SPC") 'tern-ac-complete
      ))))

(use-package emmet-mode :ensure t
  :defer t
  :config
  (setq emmet-move-cursor-between-quotes t)
  :init
  (progn
    (add-hook 'scss-mode-hook   'emmet-mode)
    (add-hook 'web-mode-hook    'emmet-mode)
    (add-hook 'haml-mode-hook   'emmet-mode)
    (add-hook 'nxml-mode-hook   'emmet-mode)))

;; Jekyll support
(define-minor-mode jekyll-mode
  :init-value nil
  :lighter " :{"
  :keymap (make-sparse-keymap))

(defun jekyll-mode-maybe()
  (let ((root (projectile-project-root)))
    (if (or (string-match "[.-]jekyll/" root)
            (file-exists-p (concat root ".jekyll-mode")))
        (jekyll-mode t))))

(nmap jekyll-mode-map (kbd ",b")
  (λ (open-file-with "http://localhost:4000")))

(add-hook 'scss-mode-hook 'jekyll-mode-maybe)
(add-hook 'web-mode-hook 'jekyll-mode-maybe)
(add-hook 'markdown-mode-hook 'jekyll-mode-maybe)

;;
(provide 'mod-webdev)
