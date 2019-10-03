;;; lang/swift/config.el -*- lexical-binding: t; -*-

(after! swift-mode
  (set-repl-handler! 'swift-mode #'run-swift))


(use-package! flycheck-swift
  :when (featurep! :tools flycheck)
  :unless (featurep! +lsp)
  :after swift-mode
  :config (flycheck-swift-setup))


(use-package! company-sourcekit
  :when (featurep! :completion company)
  :unless (featurep! +lsp)
  :after swift-mode
  :config
  (set-company-backend! 'swift-mode '(company-sourcekit company-yasnippet)))


(use-package! lsp-sourcekit
  :when (featurep! +lsp)
  :after swift-mode
  :init (add-hook 'swift-mode-hook #'lsp!))
