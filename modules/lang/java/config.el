;;; lang/java/config.el -*- lexical-binding: t; -*-

(def-package! meghanada
  :commands meghanada-mode
  :config
  (add-hook! 'java-mode-hook #'(meghanada-mode flycheck-mode))

  (set! :build 'compile-file    'java-mode #'meghanada-compile-file)
  (set! :build 'compile-project 'java-mode #'meghanada-compile-project)

  (setq meghanada-server-install-dir (concat doom-etc-dir "meghanada-server/")
        meghanada-use-company t
        meghanada-use-flycheck t
        meghanada-use-auto-start t)

  (map! :map meghanada-mode-map :m "gd" #'meghanada-jump-declaration))


(def-package! android-mode
  :commands android-mode
  :init
  (add-hook! (java-mode groovy-mode nxml-mode) #'+java|android-mode-maybe)
  :config
  (set! :yas-minor-mode 'android-mode)
  (set! :company-dict-minor-mode 'android-mode))


(def-package! groovy-mode
  :mode "\\.g\\(radle\\|roovy\\)$"
  :config
  (set! :eval 'groovy-mode "groovy"))

