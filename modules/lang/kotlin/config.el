;;; lang/kotlin/config.el -*- lexical-binding: t; -*-

(use-package! kotlin-mode
  :init 
  (when (featurep! +lsp)
    (add-hook 'kotlin-mode-local-vars-hook #'lsp!)))

(after! kotlin-mode
  (set-docsets! 'kotlin-mode "Kotlin")

  (map! :map kotlin-mode-map
        :localleader
        :prefix ("b" . "build")
        :desc "gradlew assemble" "a" (λ! (+kotlin/run-gradlew "assemble"))
        :desc "gradlew build"    "b" (λ! (+kotlin/run-gradlew "build"))
        :desc "gradlew test"     "t" (λ! (+kotlin/run-gradlew "test"))))


(use-package! flycheck-kotlin
  :when (featurep! :tools flycheck)
  :hook (kotlin-mode . flycheck-kotlin-setup))
