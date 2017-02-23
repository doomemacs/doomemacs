;;; lang/web/+css.el

(sp-with-modes '(css-mode scss-mode less-css-mode stylus-mode)
  (sp-local-pair "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC"))))

(map! :map* (css-mode-map scss-mode-map less-css-mode-map)
      :n "M-R" '+css/web-refresh-browser
      :localleader
      :nv ";" '+css/append-semicolon
      :prefix "r"
      :n  "b" '+css/toggle-inline-or-block)


;;
;; Packages
;;

(def-package! css-mode
  :mode "\\.css$"
  :mode ("\\.scss$" . scss-mode)
  :init
  (add-hook! css-mode
    '(yas-minor-mode-on flycheck-mode rainbow-mode highlight-numbers-mode
      ;; doom|counsel-css-imenu-setup
      ))

  :config
  (set! :company-backend '(css-mode scss-mode) '(company-css company-yasnippet))
  (set! :build 'scss 'scss-mode '+css/scss-build))


(def-package! sass-mode
  :mode "\\.sass$"
  :config
  (setq sass-command-options '("--style compressed"))
  (set! :build 'sass 'sass-mode '+css/sass-build)
  (set! :company-backend 'sass-mode '(company-css company-yasnippet)))


(def-package! less-css-mode
  :mode "\\.less$")


(def-package! stylus-mode
  :mode "\\.styl$"
  :init (add-hook! stylus-mode '(yas-minor-mode-on flycheck-mode)))

