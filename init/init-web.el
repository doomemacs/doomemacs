(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html)
  :init
  (after "css-mode"
    (add-hook! 'css-mode-hook (setenv "jsbeautify_indent_size" "2"))
    (bind :motion :map css-mode-map "gQ" 'web-beautify-css)))

(use-package web-mode
  :mode (("\\.\\(p\\)?htm\\(l\\)?$"         . web-mode)
         ("\\.tpl\\(\\.php\\)?$"            . web-mode)
         ("\\.erb$"                         . web-mode)
         ("wp-content/themes/.+/.+\\.php$"  . web-mode))
  :init
  (add-hook 'web-mode-hook 'narf|enable-tab-width-2)
  :config
  (progn
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
      (bind :motion :map web-mode-map "gQ" 'web-beautify-html))

    (bind :map web-mode-map
          "M-/" 'web-mode-comment-or-uncomment

          :normal
          "zf" 'web-mode-fold-or-unfold
          ",t" 'web-mode-element-rename

          :normal :visual
          "]a" 'web-mode-attribute-next
          "[a" 'web-mode-attribute-previous
          "]t" 'web-mode-tag-next
          "[t" 'web-mode-tag-previous
          "]T" 'web-mode-element-child
          "[T" 'web-mode-element-parent)))

(use-package emmet-mode
  :defer t
  :init
  (add-to-hooks 'emmet-mode '(scss-mode-hook web-mode-hook html-mode-hook haml-mode-hook nxml-mode-hook))
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (bind insert :map emmet-mode-keymap
          "M-e" 'emmet-expand-yas
          "M-E" 'emmet-expand-line)))

(define-minor-mode jekyll-mode
  "Jekyll development mode."
  :init-value nil
  :lighter " :{"
  :keymap (make-sparse-keymap)
  (narf/init-yas-mode 'jekyll-mode))
(defun narf|jekyll-mode-enable-maybe ()
  (when (narf/project-has-files '("_config.yml" "_layouts"))
    (jekyll-mode 1)))
(associate-minor-mode "/_\\(layouts\\|posts\\)/.+$" 'jekyll-mode)
(add-to-hooks 'narf|jekyll-mode-enable-maybe '(web-mode-hook scss-mode-hook html-mode-hook markdown-mode markdown-mode-hook))
(after "company" (add-to-list 'company-dictionary-major-minor-modes 'jekyll-mode))

(define-minor-mode wordpress-mode
  "Wordpress development mode."
  :init-value nil
  :lighter " wp"
  :keymap (make-sparse-keymap)
  (narf/init-yas-mode 'wordpress-mode))
(associate-minor-mode "/wp-\\(content\\|admin\\|includes\\)/.+$"  'wordpress-mode)
(associate-minor-mode "/wp-.+\\.php$"                             'wordpress-mode)
(after "company" (add-to-list 'company-dictionary-major-minor-modes 'wordpress-mode))


(provide 'init-web)
;;; init-web.el ends here
