;;; lang/qt/config.el -*- lexical-binding: t; -*-

(defun +qt-common-config (mode)
  (when (modulep! +lsp)
    (set-eglot-client! mode '("qmlls"))
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)))


(use-package! qml-mode
  :defer t
  :config
  (+qt-common-config 'qml-mode))


(use-package! qml-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'treesit-available-p)
  :defer t
  :init
  (set-tree-sitter! 'qml-mode 'qml-ts-mode
    '((qmljs :url "https://github.com/yuja/tree-sitter-qmljs")))
  :config
  (+qt-common-config 'qml-ts-mode))


(use-package! qt-pro-mode
  :mode "\\.pr[io]\\'")
