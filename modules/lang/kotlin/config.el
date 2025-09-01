;;; lang/kotlin/config.el -*- lexical-binding: t; -*-

(after! kotlin-mode
  (set-docsets! 'kotlin-mode "Kotlin")
  (set-repl-handler! 'kotlin-mode #'kotlin-repl)

  (when (modulep! +lsp)
    (add-hook 'kotlin-mode-local-vars-hook #'lsp! 'append))

  (map! :map kotlin-mode-map
        :localleader
        :prefix ("b" . "build")
        :desc "gradlew assemble" "a" (cmd! (+kotlin/run-gradlew "assemble"))
        :desc "gradlew build"    "b" (cmd! (+kotlin/run-gradlew "build"))
        :desc "gradlew test"     "t" (cmd! (+kotlin/run-gradlew "test"))))


(use-package! flycheck-kotlin
  :when (modulep! :checkers syntax -flymake)
  :hook (kotlin-mode . flycheck-kotlin-setup))


(use-package! kotlin-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'kotlin-mode 'kotlin-ts-mode
    '((kotlin :url "https://github.com/fwcd/tree-sitter-kotlin")))
  :config
  (when (modulep! +lsp)
    (add-hook 'kotlin-ts-mode-local-vars-hook #'lsp! 'append)))
