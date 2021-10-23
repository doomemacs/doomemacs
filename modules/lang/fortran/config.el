;;; lang/fortran/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! f90
  :config
  (message "Hi dad")
  (when (featurep! +lsp)
    (setq lsp-clients-fortls-args '("--enable_code_actions" "--hover_signature"))
    (add-hook 'f90-mode-local-vars-hook #'lsp!)))

(use-package! fortran
  :mode ("\\.FOR$" . fortran-mode)
  :config
  (message "Hi dad (legacy)")
  (setq flycheck-gfortran-language-standard "legacy"))
