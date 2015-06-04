(use-package sass-mode
  :mode "\\.sass$"
  :init
  (add-hook 'sass-mode-hook 'narf|enable-tab-width-2)
  :config
  (progn
    (after "company" (narf/add-company-backend sass-mode (company-css)))))

(use-package scss-mode
  :mode "\\.scss$"
  :init
  (add-hook 'scss-mode-hook 'narf|enable-tab-width-2)
  :config
  (progn
    (setq-default css-indent-offset 2)
    (setq scss-compile-at-save nil)

    ;; Syntax coloring breaks on consecutive loads for some reason. This fixes that:
    (add-hook 'scss-mode-hook 'css-mode)

    (after "web-beautify"
      (add-hook! 'scss-mode-hook (setenv "jsbeautify_indent_size" "2"))
      (bind :motion :map scss-mode-map "gQ" 'web-beautify-css))

    (after "company" (narf/add-company-backend scss-mode (company-css)))))

(use-package rainbow-mode
  :defer t
  :init (add-hook 'scss-mode-hook 'rainbow-mode))


(provide 'init-scss)
;;; init-scss.el ends here
