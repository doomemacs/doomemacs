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

  (map! :localleader
        :map java-mode-map
        (:prefix "r"
          "ia" #'meghanada-import-all
          "io" #'meghanada-optimize-import
          "l"  #'meghanada-local-variable
          "f"  #'meghanada-code-beautify)
        (:prefix "h"
          "r"  #'meghanada-reference
          "t"  #'meghanada-typeinfo)
        (:prefix "b"
          "f"  #'meghanada-compile-file
          "p"  #'meghanada-compile-project)))
