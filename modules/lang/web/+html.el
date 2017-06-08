;;; lang/web/+html.el -*- lexical-binding: t; -*-

(def-package! web-mode
  :mode "\\.p?html?$"
  :mode "\\.\\(tpl\\|blade\\)\\(\\.php\\)?$"
  :mode "\\.erb$"
  :mode "\\.jsp$"
  :mode "\\.as[cp]x$"
  :mode "\\.mustache$"
  :mode "\\.tsx$"
  :mode "wp-content/themes/.+/.+\\.php$"
  :init
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode)
  :config
  (set! :company-backend 'web-mode '(company-web-html company-yasnippet))
  (setq web-mode-enable-html-entities-fontification t)

  (map! :map web-mode-map
        (:localleader :n "rt" #'web-mode-element-rename)
        "M-/" #'web-mode-comment-or-uncomment
        :i  "SPC" #'self-insert-command
        :n  "M-r" #'doom/web-refresh-browser
        :n  "za"  #'web-mode-fold-or-unfold
        :nv "]a"  #'web-mode-attribute-next
        :nv "[a"  #'web-mode-attribute-previous
        :nv "]t"  #'web-mode-tag-next
        :nv "[t"  #'web-mode-tag-previous
        :nv "]T"  #'web-mode-element-child
        :nv "[T"  #'web-mode-element-parent))


(def-package! company-web
  :when (featurep! :completion company)
  :after web-mode)


(def-package! haml-mode :mode "\\.haml$")


(def-package! pug-mode
  :mode "\\.jade$"
  :mode "\\.pug$"
  :config
  (set! :company-backend 'pug-mode '(company-yasnippet))
  (map! :map pug-mode-map
        :i [tab] #'doom/dumb-indent
        :i [backtab] #'doom/dumb-dedent))
