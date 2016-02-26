;;; module-web.el

(add-hook! (sass-mode scss-mode) '(rainbow-mode flycheck-mode))

(use-package sass-mode
  :mode "\\.sass$"
  :config
  (after! company (define-company-backend! sass-mode (css))))

(use-package scss-mode
  :mode "\\.scss$"
  :preface
  (require 'css-mode)
  :init
  (define-docset! scss-mode "sass,bourbon")
  (add-hook! scss-mode '(hs-minor-mode narf|hl-line-off))
  (setq-default css-indent-offset 2)
  (setq scss-compile-at-save nil)
  :config
  (map! :map scss-mode-map
        :n "M-r" 'narf/web-refresh-browser
        (:leader
         :n ";" 'helm-css-scss
         :n ":" 'helm-css-scss-multi))

  (after! web-beautify
    (add-hook! scss-mode (setenv "jsbeautify_indent_size" "2"))
    (map! :map scss-mode-map :m "gQ" 'web-beautify-css))

  (after! emr
    (emr-declare-command 'narf/scss-toggle-inline-or-block
      :title "toggle inline/block"
      :modes 'scss-mode
      :predicate (lambda () (not (use-region-p)))))

  (after! company (define-company-backend! scss-mode (css))))

(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html)
  :init
  (after! css-mode
    (add-hook! css-mode (setenv "jsbeautify_indent_size" "2"))
    (map! :map css-mode-map :m "gQ" 'web-beautify-css)))

(use-package jaded-mode
  :mode "\\.jade$"
  :config
  (map! :map jaded-mode-map
        :i [tab] 'narf/dumb-indent
        :i [backtab] 'narf/dumb-dedent))

(use-package web-mode
  :mode ("\\.\\(p\\)?htm\\(l\\)?$" "\\.tpl\\(\\.php\\)?$" "\\.erb$" "wp-content/themes/.+/.+\\.php$")
  :init
  ;; smartparens handles this
  (setq web-mode-enable-auto-pairing nil
        web-mode-enable-auto-quoting nil)
  :config
  (after! web-beautify
    (add-hook! web-mode (setenv "jsbeautify_indent_size" "4"))
    (map! :map web-mode-map :m "gQ" 'web-beautify-html))

  (after! nlinum
    ;; Fix blank line numbers after unfolding
    (advice-add 'web-mode-fold-or-unfold :after 'nlinum--flush))

  (map! :map web-mode-map
        "M-/" 'web-mode-comment-or-uncomment

        :n  "za" 'web-mode-fold-or-unfold
        (:localleader :n "t" 'web-mode-element-rename)

        :n  "M-r" 'narf/web-refresh-browser

        :nv "]a" 'web-mode-attribute-next
        :nv "[a" 'web-mode-attribute-previous
        :nv "]t" 'web-mode-tag-next
        :nv "[t" 'web-mode-tag-previous
        :nv "]T" 'web-mode-element-child
        :nv "[T" 'web-mode-element-parent))

(use-package emmet-mode
  :defer t
  :init
  (add-hook! (scss-mode web-mode html-mode haml-mode nxml-mode) 'emmet-mode)
  (defvar emmet-mode-keymap (make-sparse-keymap))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (map! :map emmet-mode-keymap
        :v "M-e" 'emmet-wrap-with-markup
        :i "M-e" 'emmet-expand-yas
        :i "M-E" 'emmet-expand-line))

(define-minor-mode jekyll-mode
  "Jekyll development mode."
  :init-value nil
  :lighter " :{"
  :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'jekyll-mode))
(associate! jekyll-mode
  :match "/\\(\\(css\\|_\\(layouts\\|posts\\|sass\\)\\)/.+\\|.+.html\\)$"
  :files ("config.yml" "_layouts")
  :in (web-mode scss-mode html-mode markdown-mode))
(add-hook! jekyll-mode
  (when (eq major-mode 'web-mode)
    (web-mode-set-engine "django")))
;; (after! company-dict (add-to-list 'company-dict-minor-mode-alist 'jekyll-mode))

(define-minor-mode wordpress-mode
  "Wordpress development mode."
  :init-value nil
  :lighter " wp"
  :keymap (make-sparse-keymap)
  (add-yas-minor-mode! 'wordpress-mode))
(associate! wordpress-mode
  :match "/wp-\\(\\(content\\|admin\\|includes\\)/\\)?.+$"
  :files ("wp-config.php" "wp-content"))
;; (after! company-dict (add-to-list 'company-dict-minor-mode-alist 'wordpress-mode))

(provide 'module-web)
;;; module-web.el ends here
