;;; module-web.el

(def-company-backend! sass-mode (css))
(def-company-backend! scss-mode (css))
(def-docset! scss-mode "sass,bourbon")
(add-hook! (sass-mode scss-mode less-css-mode)
  '(flycheck-mode narf|hl-line-off hs-minor-mode))

(push '("css" "scss" "sass" "less" "styl") projectile-other-file-alist)

(use-package haml-mode :mode "\\.haml$")

(use-package stylus-mode :mode "\\.styl$"
  :config (push '("styl" "css") projectile-other-file-alist))

(use-package less-css-mode :mode "\\.less$"
  :config (push '("less" "css") projectile-other-file-alist))

(use-package sass-mode :mode "\\.sass$"
  :config (push '("sass" "css") projectile-other-file-alist))

(use-package scss-mode
  :mode "\\.scss$"
  :preface (require 'css-mode)
  :init (setq scss-compile-at-save nil)
  :config
  (push '("scss" "css") projectile-other-file-alist)
  (sp-local-pair 'scss-mode "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))

  (map! :map scss-mode-map
        :n "M-r" 'narf/web-refresh-browser
        (:localleader :nv ";" 'narf/append-semicolon)
        (:leader
          :n ";" 'helm-css-scss
          :n ":" 'helm-css-scss-multi))

  (after! emr
    (emr-declare-command 'narf/scss-toggle-inline-or-block
      :title "toggle inline/block"
      :modes 'scss-mode
      :predicate (lambda () (not (use-region-p))))))

(use-package jaded-mode
  :load-path "/Volumes/hlissner/Dropbox/work/plugins/jaded-mode"
  :mode "\\.jade$"
  :config
  (push '("jade" "html") projectile-other-file-alist)
  (map! :map jaded-mode-map
        :i [tab] 'narf/dumb-indent
        :i [backtab] 'narf/dumb-dedent))

(use-package web-mode
  :mode ("\\.p?html?$"
         "\\.\\(tpl\\|blade\\)\\(\\.php\\)?$"
         "\\.erb$"
         "\\.as[cp]x$"
         "\\.mustache$"
         "wp-content/themes/.+/.+\\.php$")
  :init
  (add-hook 'web-mode-hook 'turn-off-smartparens-mode)

  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-enable-current-column-highlight t)
  (push '("html" "jade") projectile-other-file-alist)

  (map! :map web-mode-map :i "SPC" 'self-insert-command)

  (after! nlinum
    ;; Fix blank line numbers after unfolding
    (advice-add 'web-mode-fold-or-unfold :after 'nlinum--flush))

  (map! :map web-mode-map
        "M-/" 'web-mode-comment-or-uncomment
        :n  "M-r" 'narf/web-refresh-browser

        :n  "za" 'web-mode-fold-or-unfold
        (:localleader :n "t" 'web-mode-element-rename)

        :nv "]a" 'web-mode-attribute-next
        :nv "[a" 'web-mode-attribute-previous
        :nv "]t" 'web-mode-tag-next
        :nv "[t" 'web-mode-tag-previous
        :nv "]T" 'web-mode-element-child
        :nv "[T" 'web-mode-element-parent))

;;
(use-package emmet-mode
  :commands (emmet-mode)
  :init
  (add-hook! (scss-mode web-mode html-mode haml-mode nxml-mode) 'emmet-mode)
  (defvar emmet-mode-keymap (make-sparse-keymap))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (map! :map emmet-mode-keymap
        :v "M-e" 'emmet-wrap-with-markup
        :i "M-e" 'emmet-expand-yas
        :i "M-E" 'emmet-expand-line))

;;
(def-project-type! jekyll ":{"
  :modes (web-mode scss-mode html-mode markdown-mode yaml-mode)
  :match "/\\(\\(css\\|_\\(layouts\\|posts\\|sass\\)\\)/.+\\|.+.html\\)$"
  :files ("config.yml" "_layouts/")
  (add-hook! mode
    (when (eq major-mode 'web-mode)
      (web-mode-set-engine "django"))))

(def-project-type! wordpress "wp"
  :modes (php-mode web-mode css-mode scss-mode sass-mode)
  :match "/wp-\\(\\(content\\|admin\\|includes\\)/\\)?.+$"
  :files ("wp-config.php" "wp-content/"))

;; TODO Add stylus-mode

(provide 'module-web)
;;; module-web.el ends here
