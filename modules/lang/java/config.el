;;; lang/java/config.el -*- lexical-binding: t; -*-

(add-hook 'java-mode-hook #'rainbow-delimiters-mode)

(cond ((featurep! +meghanada) (load! "+meghanada"))
      ;; TODO lang/java +lsp (lsp-java?)
      ;; ((featurep! +lsp) (load! "+lsp"))
      )


;;
;; Common packages

(def-package! android-mode
  :commands android-mode
  :init
  (add-hook! (java-mode groovy-mode nxml-mode) #'+java|android-mode-maybe)
  :config
  (set-yas-minor-mode! 'android-mode))


(def-package! groovy-mode
  :mode "\\.g\\(?:radle\\|roovy\\)$"
  :config
  (set-eval-handler! 'groovy-mode "groovy"))

