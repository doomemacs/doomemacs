;;; lang/java/+meghanada.el -*- lexical-binding: t; -*-
;;;###if (featurep! +meghanada)

(def-package! meghanada
  :hook (java-mode . meghanada-mode)
  :init
  (setq meghanada-server-install-dir (concat doom-etc-dir "meghanada-server/")
        meghanada-use-company (featurep! :completion company)
        meghanada-use-flycheck (featurep! :feature syntax-checker)
        meghanada-use-eldoc t
        meghanada-use-auto-start t)
  :config
  (set-lookup-handlers! 'java-mode
    :definition #'meghanada-jump-declaration
    :references #'meghanada-reference)

  (map! :map java-mode-map
        :localleader
        (:prefix "r"
          :n "ia" #'meghanada-import-all
          :n "io" #'meghanada-optimize-import
          :n "l"  #'meghanada-local-variable
          :n "f"  #'meghanada-code-beautify)
        (:prefix "h"
          :n "r"  #'meghanada-reference
          :n "t"  #'meghanada-typeinfo)
        (:prefix "b"
          :n "f"  #'meghanada-compile-file
          :n "p"  #'meghanada-compile-project)))
