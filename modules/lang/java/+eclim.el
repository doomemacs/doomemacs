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

  (map! :map java-mode-map
        :localleader
        (:prefix "r"
          :n "gc" #'eclim-java-constructor
          :n "gg" #'eclim-java-generate-getter-and-setter
          :n "oi" #'eclim-java-import-organize
          :n "f"  #'eclim-java-format
          :n "r"  #'eclim-java-refactor-rename-symbol-at-point)
        (:prefix "h"
          :n "."  #'eclim-java-show-documentation-for-current-element
          :n "r"  #'eclim-java-find-references
          :n "c"  #'eclim-java-call-hierarchy
          :n "h"  #'eclim-java-hierarchy
          :n "p"  #'eclim-problems
          :n "r"  #'meghanada-reference
          :n "t"  #'meghanada-typeinfo)
        (:prefix "b"
          :n "b"  #'eclim-project-build
          :n "c"  #'eclim-project-create
          :n "d"  #'eclim-project-delete
          :n "g"  #'eclim-project-goto
          :n "i"  #'eclim-project-import
          :n "k"  #'eclim-project-close
          :n "o"  #'eclim-project-open
          :n "u"  #'eclim-project-update)))


(def-package! company-emacs-eclim
  :when (featurep! :completion company)
  :after java-mode
  :config
  (set-company-backend! 'java-mode '(company-emacs-eclim)))
