;;; lang/swift/config.el -*- lexical-binding: t; -*-

(defun +swift-common-config (mode)
  (set-repl-handler! mode #'run-swift)
  (set-eglot-client! mode '("sourcekit-lsp"))

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)))


(use-package! swift-mode
  :defer t
  :config
  (+swift-common-config 'swift-mode))


(use-package! swift-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'swift-mode 'swift-ts-mode
    '((swift :url "https://github.com/alex-pinkus/tree-sitter-swift"
             :rev "0.7.1-with-generated-files")))
  :config
  (+swift-common-config 'swift-ts-mode))


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
  :config
  (set-formatter! 'swiftformat '("swiftformat" "--output" "stdout"))
  (setq lsp-sourcekit-executable
        (cl-find-if #'executable-find
                    (list lsp-sourcekit-executable ; 'sourcekit-lsp' by default
                          "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"
                          "sourcekit"
                          "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit-lsp"
                          "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit"))))
