;;; lang/java/+eclim.el -*- lexical-binding: t; -*-
;;;###if (featurep! +eclim)

;; NOTE This submodule is incomplete

(def-package! eclim
  :hook (java-mode . eclim-mode)
  :config
  (set-lookup-handlers! 'java-mode
    :definition #'eclim-java-find-declaration
    :references #'eclim-java-find-references
    :documentation #'eclim-java-show-documentation-for-current-element)

  (require 'eclimd)
  (setq help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  (map! :localleader
        :map java-mode-map
        (:prefix "r"
          "gc" #'eclim-java-constructor
          "gg" #'eclim-java-generate-getter-and-setter
          "oi" #'eclim-java-import-organize
          "f"  #'eclim-java-format
          "r"  #'eclim-java-refactor-rename-symbol-at-point)
        (:prefix "h"
          "."  #'eclim-java-show-documentation-for-current-element
          "r"  #'eclim-java-find-references
          "c"  #'eclim-java-call-hierarchy
          "h"  #'eclim-java-hierarchy
          "p"  #'eclim-problems
          "r"  #'meghanada-reference
          "t"  #'meghanada-typeinfo)
        (:prefix "b"
          "b"  #'eclim-project-build
          "c"  #'eclim-project-create
          "d"  #'eclim-project-delete
          "g"  #'eclim-project-goto
          "i"  #'eclim-project-import
          "k"  #'eclim-project-close
          "o"  #'eclim-project-open
          "u"  #'eclim-project-update)))


(def-package! company-emacs-eclim
  :when (featurep! :completion company)
  :after java-mode
  :config
  (set-company-backend! 'java-mode '(company-emacs-eclim)))
