;;; module-web.el

(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html)
  :init
  (after! css-mode
    (add-hook! css-mode (setenv "jsbeautify_indent_size" "2"))
    (bind! :map css-mode-map :m "gQ" 'web-beautify-css)))

(use-package jade-mode
  :mode "\\.jade$"
  :init (add-hook! jade-mode 'narf|enable-tab-width-2)
  :config (require 'sws-mode))

(use-package web-mode
  :mode ("\\.\\(p\\)?htm\\(l\\)?$" "\\.tpl\\(\\.php\\)?$" "\\.erb$" "wp-content/themes/.+/.+\\.php$")
  :init
  (add-hook! web-mode 'narf|enable-tab-width-2)
  (setq web-mode-markup-indent-offset  2
        web-mode-code-indent-offset    2
        web-mode-css-indent-offset     2
        web-mode-style-padding         2
        web-mode-script-padding        2
        web-mode-block-padding         2)
  :config
  (after! web-beautify
    (add-hook! web-mode (setenv "jsbeautify_indent_size" "4"))
    (bind! :map web-mode-map :m "gQ" 'web-beautify-html))

  (after! nlinum
    ;; Fix blank line numbers after unfolding
    (advice-add 'web-mode-fold-or-unfold :after 'nlinum--flush))

  (bind! :map web-mode-map
         "M-/" 'web-mode-comment-or-uncomment

         :n  "za" 'web-mode-fold-or-unfold
         :n  ",t" 'web-mode-element-rename

         :n  "M-r" 'narf/web-refresh-browser

         :nv "]a" 'web-mode-attribute-next
         :nv "[a" 'web-mode-attribute-previous
         :nv "]t" 'web-mode-tag-next
         :nv "[t" 'web-mode-tag-previous
         :nv "]T" 'web-mode-element-child
         :nv "[T" 'web-mode-element-parent))

(use-package emmet-mode
  :defer t
  :diminish emmet-mode
  :init (add-hook! (scss-mode web-mode html-mode haml-mode nxml-mode) 'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (bind! :map emmet-mode-keymap
         :i "M-e" 'emmet-expand-yas
         :i "M-E" 'emmet-expand-line))

(define-minor-mode jekyll-mode
  "Jekyll development mode."
  :init-value nil
  :lighter " :{"
  :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'jekyll-mode))
(associate! jekyll-mode
  :match "/_\\(layouts\\|posts\\)/.+$"
  :files ("config.yml" "_layouts")
  :in (web-mode scss-mode html-mode markdown-mode))
;; (after! company-dict (add-to-list 'company-dict-minor-mode-alist 'jekyll-mode))

(define-minor-mode wordpress-mode
  "Wordpress development mode."
  :init-value nil
  :lighter " wp"
  :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'wordpress-mode))
(associate! wordpress-mode :match "/wp-\\(content\\|admin\\|includes\\)/.+$")
(associate! wordpress-mode :match "/wp-.+\\.php$")
;; (after! company-dict (add-to-list 'company-dict-minor-mode-alist 'wordpress-mode))

(provide 'module-web)
;;; module-web.el ends here
