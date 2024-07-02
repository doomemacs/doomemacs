;;; lang/idris2/config.el -*- lexical-binding: t; -*-

(use-package! idris2-mode
  :mode ("\\.l?idr\\'" . idris2-mode)
  :config

  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(idris2-mode . "idris2"))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "idris2-lsp")
      :major-modes '(idris2-mode)
      :server-id 'idris2-lsp)))

  (add-hook 'idris2-mode-hook #'lsp!))
