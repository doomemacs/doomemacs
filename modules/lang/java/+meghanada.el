;;; lang/java/+meghanada.el -*- lexical-binding: t; -*-
;;;###if (featurep! +meghanada)

(def-package! meghanada
  :hook (java-mode . meghanada-mode)
  :config
  (setq meghanada-server-install-dir (concat doom-etc-dir "meghanada-server/")
        meghanada-use-company (featurep! :completion company)
        meghanada-use-flycheck (featurep! :feature syntax-checker)
        meghanada-use-eldoc t
        meghanada-use-auto-start t)

  (set! :jump 'java-mode
    :definition #'meghanada-jump-declaration
    :references #'meghanada-reference)

  (add-hook! 'meghanada-mode-hook #'(flycheck-mode eldoc-mode))

  ;;
  (def-menu! +java/refactor-menu
    "Refactoring commands for `java-mode' buffers."
    '(("Add imports for unqualified classes" :exec meghanada-import-all)
      ("Optimize and clean up imports" :exec meghanada-optimize-import)
      ("Introduce local variable" :exec meghanada-local-variable)
      ("Format buffer code" :exec meghanada-code-beautify)))

  (def-menu! +java/help-menu
    "Help and information commands for `java-mode' buffers."
    '(("Find usages" :exec meghanada-reference)
      ("Show type hierarchives and implemented interfaces" :exec meghanada-typeinfo)))

  (def-menu! +java/project-menu
    "Project commands for `java-mode' buffers."
    '(("Compile current file" :exec meghanada-compile-file)
      ("Compile project" :exec meghanada-compile-project)))

  (map! :map java-mode-map
        :localleader
        :nv "r" #'+java/refactor-menu
        :nv "c" #'+java/compile-menu
        :nv "p" #'+java/project-menu))
