;;; lang/swift/config.el -*- lexical-binding: t; -*-

(after! swift-mode
  (set-repl-handler! 'swift-mode #'run-swift))


(def-package! flycheck-swift
  :when (and (featurep! :tools flycheck)
             (not (featurep! +lsp)))
  :after swift-mode
  :config (flycheck-swift-setup))


(def-package! company-sourcekit
  :when (and (featurep! :completion company)
             (not (featurep! +lsp)))
  :after swift-mode
  :config
  (set-company-backend! 'swift-mode '(company-sourcekit company-yasnippet)))


(def-package! lsp-sourcekit
  :when (featurep! +lsp)
  :after swift-mode
  :init (add-hook 'swift-mode-hook #'lsp!))
