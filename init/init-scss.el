(use-package scss-mode
  :mode "\\.scss$"
  :config
  (progn
    (add-hook 'scss-mode-hook 'enable-tab-width-2)

    (setq-default css-indent-offset 2)
    (setq scss-compile-at-save nil)

    (after "web-beautify"
      (add-hook! 'scss-mode-hook (setenv "jsbeautify_indent_size" "2"))
      (bind 'motion scss-mode-map "gQ" 'web-beautify-css))

    (after "auto-complete" (add-hook 'scss-mode-hook 'ac-css-mode-setup))
    (after "company" (company--backend-on 'scss-mode-hook 'company-css))))

(use-package rainbow-mode
  :defer t
  :init (add-hook 'scss-mode-hook 'rainbow-mode))


(provide 'init-scss)
;;; init-scss.el ends here
