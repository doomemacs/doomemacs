;;; lang/java/+meghanada.el -*- lexical-binding: t; -*-

(def-package! meghanada
  :commands meghanada-mode
  :init
  (add-hook! 'java-mode-hook #'(meghanada-mode rainbow-delimiters-mode))
  :config
  (setq meghanada-server-install-dir (concat doom-etc-dir "meghanada-server/")
        meghanada-use-company (featurep! :completion company)
        meghanada-use-flycheck (featurep! :feature syntax-checker)
        meghanada-use-eldoc t
        meghanada-use-auto-start t)

  ;; Setup on first use
  (meghanada-install-server)
  (if (file-exists-p (meghanada--locate-server-jar))
      (add-hook! 'meghanada-mode-hook #'(flycheck-mode eldoc-mode))
    (warn "java-mode: meghanada-server not installed, java-mode will run with reduced functionality"))

  (set! :jump 'java-mode
    :definition #'meghanada-jump-declaration
    :references #'meghanada-reference)

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
