;;; lang/qt/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pr[io]\\'" . qt-pro-mode))

;;;###autoload
(when (modulep! +tree-sitter)
  (set-tree-sitter! 'qml-mode 'qml-ts-mode
    '((qmljs :url "https://github.com/yuja/tree-sitter-qmljs"))))

;;;###autoload
(when (modulep! +lsp)
  (add-hook 'qml-mode-local-vars-hook #'lsp! 'append)
  (add-hook 'qml-ts-mode-local-vars-hook #'lsp! 'append))
