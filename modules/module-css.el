;;; module-css.el

(after! emr
  (emr-declare-command 'narf/css-toggle-inline-or-block
    :title "toggle inline/block"
    :modes '(css-mode less-css-mode scss-mode)
    :predicate (lambda () (not (use-region-p)))))

(sp-with-modes '(css-mode scss-mode less-css-mode stylus-mode)
  (sp-local-pair "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC"))))

(use-package css-mode
  :mode "\\.css$"
  :init (add-hook! css-mode '(yas-minor-mode-on flycheck-mode rainbow-mode))
  :config
  (def-company-backend! css-mode (css yasnippet))
  (push '("css" "scss" "sass" "less" "styl") projectile-other-file-alist))

(use-package stylus-mode
  :mode "\\.styl$"
  :init (add-hook! stylus-mode '(yas-minor-mode-on flycheck-mode))
  :config (push '("styl" "css") projectile-other-file-alist))

(use-package less-css-mode
  :mode "\\.less$"
  :config (push '("less" "css") projectile-other-file-alist))


;;
;; Sass
;;

(setq scss-sass-options '("--style" "compressed"))

(use-package sass-mode
  :mode "\\.sass$"
  :config
  (def-builder! sass-mode narf/sass-build)
  (def-company-backend! sass-mode (css yasnippet))
  (def-docset! sass-mode "sass,bourbon")
  (push '("sass" "css") projectile-other-file-alist))

(use-package scss-mode
  :mode "\\.scss$"
  :config
  (def-builder! scss-mode narf/scss-build)
  (def-company-backend! scss-mode (css yasnippet))
  (def-docset! scss-mode "sass,bourbon")
  (push '("scss" "css") projectile-other-file-alist)
  (setq scss-compile-at-save nil))

(map! :map (css-mode-map sass-mode-map scss-mode-map)
      :n "M-R" 'narf/web-refresh-browser
      (:localleader :nv ";" 'narf/append-semicolon)
      (:leader
        :n ";" 'helm-css-scss
        :n ":" 'helm-css-scss-multi))

(provide 'module-css)
;;; module-css.el ends here
