;;; module-css.el

(after! emr
  (emr-declare-command 'doom/css-toggle-inline-or-block
    :title "toggle inline/block"
    :modes '(css-mode less-css-mode scss-mode)
    :predicate (lambda () (not (use-region-p)))))

(sp-with-modes '(css-mode scss-mode less-css-mode stylus-mode)
  (sp-local-pair "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC"))))

(use-package css-mode
  :mode "\\.css$"
  :init (add-hook! css-mode '(yas-minor-mode-on flycheck-mode rainbow-mode highlight-numbers-mode))
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

(use-package counsel-css
  :commands (counsel-css counsel-css-imenu-setup)
  :init (add-hook 'css-mode-hook 'counsel-css-imenu-setup))


;;
;; Sass
;;

(setq scss-sass-options '("--style" "compressed"))

(use-package sass-mode
  :mode "\\.sass$"
  :config
  (def-builder! sass-mode doom/sass-build)
  (def-company-backend! sass-mode (css yasnippet))
  (push '("sass" "css") projectile-other-file-alist))

(use-package scss-mode
  :mode "\\.scss$"
  :config
  (def-builder! scss-mode doom/scss-build)
  (def-company-backend! scss-mode (css yasnippet))
  (def-docset! scss-mode "sass,bourbon,compass,neat,css")
  (push '("scss" "css") projectile-other-file-alist)
  (setq scss-compile-at-save nil))

(map! :map (css-mode-map sass-mode-map scss-mode-map)
      :n "M-R" 'doom/web-refresh-browser
      (:localleader :nv ";" 'doom/append-semicolon))

(provide 'module-css)
;;; module-css.el ends here
