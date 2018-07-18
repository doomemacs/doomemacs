;;; lang/swift/config.el -*- lexical-binding: t; -*-

(after! swift-mode
  (set-repl-handler! 'swift-mode #'run-swift))


(def-package! flycheck-swift
  :when (featurep! :feature syntax-checker)
  :after swift-mode
  :config (flycheck-swift-setup))


(def-package! company-sourcekit
  :when (featurep! :completion company)
  :after swift-mode
  :config
  (set-company-backend! 'swift-mode '(company-sourcekit company-yasnippet)))

