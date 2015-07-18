;;; module-sass.el --- sass/scss

(use-package sass-mode
  :mode "\\.sass$"
  :init   (add-hook! sass-mode 'narf|enable-tab-width-2)
  :config (after! company (add-company-backend! sass-mode (css))))

(use-package scss-mode
  :mode "\\.scss$"
  :init
  (add-hook! scss-mode 'narf|enable-tab-width-2)
  (add-hook! scss-mode 'hs-minor-mode)
  (setq-default css-indent-offset 2)
  (setq scss-compile-at-save nil)
  :config
  (after! web-beautify
    (add-hook! scss-mode (setenv "jsbeautify_indent_size" "2"))
    (bind! :map scss-mode-map :m "gQ" 'web-beautify-css))

  (after! emr
    (emr-declare-command 'narf/scss-toggle-inline-or-block
      :title "toggle inline/block"
      :modes 'scss-mode
      :predicate (lambda () (not (use-region-p)))))

  (after! company (add-company-backend! scss-mode (css))))

(use-package rainbow-mode
  :diminish rainbow-mode
  :defer t
  :init (add-hook! scss-mode 'rainbow-mode))

(provide 'module-sass)
;;; module-sass.el ends here
