;;; lang/kotlin/config.el -*- lexical-binding: t; -*-

(after! kotlin-mode
  (when (featurep! +lsp)
    (add-hook 'kotlin-mode-local-vars-hook #'lsp!))
  (set-docsets! 'kotlin-mode "Kotlin")
  (set-repl-handler! 'kotlin-mode #'kotlin-repl)

  (map! :map kotlin-mode-map
        :localleader
        :prefix ("b" . "build")
        :desc "gradlew assemble" "a" (λ! (+kotlin/run-gradlew "assemble"))
        :desc "gradlew build"    "b" (λ! (+kotlin/run-gradlew "build"))
        :desc "gradlew test"     "t" (λ! (+kotlin/run-gradlew "test"))))


(use-package! flycheck-kotlin
  :when (featurep! :checkers syntax)
  :hook (kotlin-mode . flycheck-kotlin-setup))
