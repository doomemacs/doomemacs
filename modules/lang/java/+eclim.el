;;; lang/java/+eclim.el -*- lexical-binding: t; -*-
;;;###if (featurep! +eclim)

;; NOTE This submodule is incomplete

(def-package! eclim
  :hook (java-mode . eclim-mode)
  :config
  (set! :jump 'java-mode
    :definition #'eclim-java-find-declaration
    :references #'eclim-java-find-references
    :documentation #'eclim-java-show-documentation-for-current-element)

  (require 'eclimd)
  (setq help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  ;;
  (def-menu! +java/refactor-menu
    "Refactoring commands for `java-mode' buffers."
    '(("Generate constructor"     :exec eclim-java-constructor)
      ("Generate getter & setter" :exec eclim-java-generate-getter-and-setter)
      ("Organize imports"         :exec eclim-java-import-organize)
      ("Reformat"                 :exec eclim-java-format)
      ("Rename symbol at point"   :exec eclim-java-refactor-rename-symbol-at-point :region nil)))

  (def-menu! +java/help-menu
    "Help and information commands for `java-mode' buffers."
    '(("Find documentation for current element" :exec eclim-java-show-documentation-for-current-element)
      ("Find references"     :exec eclim-java-find-references)
      ("View call hierarchy" :exec eclim-java-call-hierarchy)
      ("View hierarchy"      :exec eclim-java-hierarchy)
      ("View problems"       :exec eclim-problems)))

  (def-menu! +java/project-menu
    "Building/compilation commands for `java-mode' buffers."
    '(("Build project"  :exec eclim-project-build)
      ("Create project" :exec eclim-project-create)
      ("Delete project" :exec eclim-project-delete)
      ("Go to project"  :exec eclim-project-goto)
      ("Import project" :exec eclim-project-import)
      ("Close project"  :exec eclim-project-close)
      ("Open project"   :exec eclim-project-open)
      ("Update project" :exec eclim-project-update)))

  (map! :map java-mode-map
        :localleader
        "r" #'+java/refactor-menu
        "c" #'+java/compile-menu
        "p" #'+java/project-menu))


(def-package! company-emacs-eclim
  :when (featurep! :completion company)
  :after java-mode
  :config
  (set! :company-backend 'java-mode '(company-emacs-eclim)))
