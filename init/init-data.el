(use-package yaml-mode
  :mode "\\.ya?ml$"
  :init (add-hook 'yaml-mode-hook 'narf|enable-tab-width-2))

(use-package json-mode
  :mode (("\\.json$" . json-mode)
         ("\\.jshintrc$" . json-mode)))


(provide 'init-data)
;;; init-data.el ends here
