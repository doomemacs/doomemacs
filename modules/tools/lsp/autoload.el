;;; feature/lsp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +lsp|init ()
  "Enable LSP as late as possible, to allow users to customize it via file or
dir local variables."
  (add-hook 'hack-local-variables-hook #'lsp nil t))
