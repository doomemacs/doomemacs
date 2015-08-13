;;; module-data.el --- dbs 'n data formats

(use-package yaml-mode
  :mode "\\.ya?ml$"
  :init (add-hook! yaml-mode 'narf|enable-tab-width-2))

(use-package json-mode :mode "\\.js\\(on\\|hintrc\\)$")

;; TODO: Db client

(provide 'module-data)
;;; module-data.el ends here
