(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html)
  :init
  (after "css-mode"
    (add-hook! 'css-mode-hook (setenv "jsbeautify_indent_size" "2"))
    (bind 'motion css-mode-map "gQ" 'web-beautify-css)))

(use-package web-mode
  :mode (("\\.\\(p\\)?htm\\(l\\)?$" . web-mode)
         ("\\.tpl\\(\\.php\\)?$" . web-mode)
         ("\\.erb$" . web-mode)
         ("wp-content/themes/.+/.+\\.php$" . web-mode))
  :config
  (progn
    (add-hook 'web-mode-hook 'enable-tab-width-2)

    (setq web-mode-markup-indent-offset  2
          web-mode-code-indent-offset    2
          web-mode-css-indent-offset     2
          web-mode-style-padding         2
          web-mode-script-padding        2
          web-mode-block-padding         2)

    (after "smartparens"
      (add-hook! 'web-mode-hook (setq web-mode-enable-auto-pairing nil))

      (defun sp-web-mode-is-code-context (id action context)
        (when (and (eq action 'insert)
                   (not (or (get-text-property (point) 'part-side)
                            (get-text-property (point) 'block-side))))
          t))

      (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))

    (after "web-beautify"
      (add-hook! 'web-mode-hook (setenv "jsbeautify_indent_size" "4"))
      (bind 'motion web-mode-map "gQ" 'web-beautify-html))

    (bind web-mode-map (kbd "M-/") 'web-mode-comment-or-uncomment)
    (bind 'normal  web-mode-map
          "zf" 'web-mode-fold-or-unfold
          ",t" 'web-mode-element-rename)
    (bind '(normal visual) web-mode-map
          "]a" 'web-mode-attribute-next
          "[a" 'web-mode-attribute-previous
          "]t" 'web-mode-tag-next
          "[t" 'web-mode-tag-previous
          "]T" 'web-mode-element-child
          "[T" 'web-mode-element-parent)))

(use-package emmet-mode
  :defer t
  :init
  (progn
    (add-hook 'scss-mode-hook   'emmet-mode)
    (add-hook 'web-mode-hook    'emmet-mode)
    (add-hook 'html-mode-hook   'emmet-mode)
    (add-hook 'haml-mode-hook   'emmet-mode)
    (add-hook 'nxml-mode-hook   'emmet-mode))
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (bind 'insert emmet-mode-keymap
          "M-e" 'emmet-expand-yas
          "M-E" 'emmet-expand-line)))

(define-minor-mode jekyll-mode
  :init-value nil
  :lighter " :{"
  :keymap (make-sparse-keymap)
  (my--init-yas-mode 'jekyll-mode))
(defun jekyll-mode-enable-maybe ()
  (when (project-has-files '("_config.yml" "_layouts"))
    (jekyll-mode 1)))
(associate-minor-mode "/_\\(layouts\\|posts\\)/.+$" 'jekyll-mode)
(add-hooks '(web-mode-hook scss-mode-hook html-mode-hook markdown-mode markdown-mode-hook)
           'jekyll-mode-enable-maybe)
(after "company" (add-to-list 'company-dictionary-major-minor-modes 'jekyll-mode))

(define-minor-mode wordpress-mode
  :init-value nil
  :lighter " wp"
  :keymap (make-sparse-keymap)
  (my--init-yas-mode 'wordpress-mode))
(associate-minor-mode "/wp-\\(content\\|admin\\|includes\\)/.+$" 'wordpress-mode)
(associate-minor-mode "/wp-.+\\.php$" 'wordpress-mode)
(after "company" (add-to-list 'company-dictionary-major-minor-modes 'wordpress-mode))


(provide 'init-web)
;;; init-web.el ends here
