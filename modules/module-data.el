;;; module-data.el --- dbs 'n data formats

(add-hook! (yaml-mode toml-mode) 'narf|enable-tab-width-2)

(use-package yaml-mode :mode "\\.ya?ml$")

(use-package json-mode :mode "\\.js\\(on\\|hintrc\\)$")

(use-package toml-mode :mode "\\.toml$")

;; TODO: Db client

(provide 'module-data)
;;; module-data.el ends here
