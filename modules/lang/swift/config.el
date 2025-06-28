;;; lang/swift/config.el -*- lexical-binding: t; -*-

(use-package! swift-mode
  :defer t
  :init
  (when (modulep! +tree-sitter)
    (set-tree-sitter! 'swift-mode 'swift-ts-mode
      '((swift :url "https://github.com/alex-pinkus/tree-sitter-swift"))))

  :config
  (set-repl-handler! 'swift-mode #'run-swift)
  (set-eglot-client! 'swift-mode '("sourcekit-lsp"))

  (when (modulep! +lsp)
    (add-hook 'swift-mode-local-vars-hook #'lsp! 'append)))


(use-package! flycheck-swift
  :when (modulep! :checkers syntax)
  :unless (modulep! +lsp)
  :after swift-mode
  :config (flycheck-swift-setup))


(use-package! company-sourcekit
  :when (modulep! :completion company)
  :unless (modulep! +lsp)
  :after swift-mode
  :config
  (set-company-backend! 'swift-mode '(company-sourcekit company-yasnippet)))


(use-package! lsp-sourcekit
  :when (modulep! +lsp)
  :when (modulep! :tools lsp -eglot)
  :defer t
  :init (add-hook 'swift-mode-local-vars-hook #'lsp! 'append)
  :config
  (set-formatter! 'swiftformat '("swiftformat" "--output" "stdout"))
  (setq lsp-sourcekit-executable
        (cl-find-if #'executable-find
                    (list lsp-sourcekit-executable ; 'sourcekit-lsp' by default
                          "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"
                          "sourcekit"
                          "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit-lsp"
                          "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit"))))
