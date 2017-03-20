;;; lang/web/config.el

(def-package! web-mode
  :mode ("\\.p?html?$"
         "\\.\\(tpl\\|blade\\)\\(\\.php\\)?$"
         "\\.erb$"
         "\\.jsp$"
         "\\.as[cp]x$"
         "\\.mustache$"
         "\\.tsx$"
         "wp-content/themes/.+/.+\\.php$")
  :init
  (add-hook 'web-mode-hook 'turn-off-smartparens-mode)
  :config
  (set! :company-backend 'web-mode '(company-web-html company-yasnippet))
  (setq web-mode-enable-html-entities-fontification t)

  ;; Fix blank line numbers after unfolding
  (advice-add 'web-mode-fold-or-unfold :after 'nlinum--flush)

  (map! :map web-mode-map
        (:localleader :n "rt" 'web-mode-element-rename)
        "M-/" 'web-mode-comment-or-uncomment
        :i  "SPC" 'self-insert-command
        :n  "M-r" 'doom/web-refresh-browser
        :n  "za"  'web-mode-fold-or-unfold
        :nv "]a"  'web-mode-attribute-next
        :nv "[a"  'web-mode-attribute-previous
        :nv "]t"  'web-mode-tag-next
        :nv "[t"  'web-mode-tag-previous
        :nv "]T"  'web-mode-element-child
        :nv "[T"  'web-mode-element-parent))


(def-package! company-web
  :when (featurep! :completion company)
  :after web-mode)


(def-package! haml-mode :mode "\\.haml$")


(def-package! pug-mode
  :mode ("\\.jade$" "\\.pug$")
  :config
  (set! :company-backend 'pug-mode '(company-yasnippet))
  (map! :map pug-mode-map
        :i [tab] 'doom/dumb-indent
        :i [backtab] 'doom/dumb-dedent))


;;
;; Tools
;;

(def-package! emmet-mode
  :commands emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :init
  (add-hook! (scss-mode web-mode html-mode haml-mode nxml-mode) 'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (map! :map emmet-mode-keymap
        :v "M-e" 'emmet-wrap-with-markup
        :i "M-e" 'emmet-expand-yas
        :i "M-E" 'emmet-expand-line))
