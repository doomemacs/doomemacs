(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html)
  :init
  (after "css-mode"
    (add-hook! 'css-mode-hook (setenv "jsbeautify_indent_size" "2"))
    (bind 'motion css-mode-map "gQ" 'web-beautify-css)))

(use-package web-mode
  :commands web-mode
  :mode (("\\.\\(p\\)?htm\\(l\\)?$" . web-mode)
         ("\\.tpl\\(\\.php\\)?$" . web-mode)
         ("\\.erb$" . web-mode)
         ("wp-content/themes/.+/.+\\.php$" . web-mode))
  :config
  (progn
    (add-hook 'web-mode-hook 'enable-tab-width-2)

    (setq web-mode-ac-sources-alist '(("css" . (ac-source-css-property)))
          web-mode-markup-indent-offset  2
          web-mode-code-indent-offset    2
          web-mode-css-indent-offset     2
          web-mode-style-padding         2
          web-mode-script-padding        2
          web-mode-block-padding         2)

    (after "web-beautify"
      (add-hook! 'web-mode-hook (setenv "jsbeautify_indent_size" "4"))
      (bind 'motion web-mode-map "gQ" 'web-beautify-html))

    (bind web-mode-map (kbd "s-/") 'web-mode-comment-or-uncomment)
    (bind 'normal  web-mode-map
          "zf" 'web-mode-fold-or-unfold
          ",t" 'web-mode-element-rename)
    (bind '(normal visual) web-mode-map
          "]a" 'web-mode-attribute-next
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
          (kbd "s-e") 'emmet-expand-yas
          (kbd "s-E") 'emmet-expand-line)))


(provide 'init-web)
;;; init-web.el ends here
