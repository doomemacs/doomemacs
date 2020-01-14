;;; lang/swift/config.el -*- lexical-binding: t; -*-

(after! swift-mode
  (set-repl-handler! 'swift-mode #'run-swift))


(use-package! flycheck-swift
  :when (featurep! :checkers syntax)
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
  :init (add-hook 'swift-mode-local-vars-hook #'lsp!)
  :config
  (unless (getenv "SOURCEKIT_TOOLCHAIN_PATH")
    (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain"))
  (setq lsp-sourcekit-executable
        (cl-find-if #'executable-find
                    (list lsp-sourcekit-executable ; 'sourcekit' by default
                          "sourcekit-lsp"
                          "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit"
                          "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit-lsp"))))
