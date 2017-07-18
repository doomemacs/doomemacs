;;; lang/java/config.el -*- lexical-binding: t; -*-

(def-package! meghanada
  :commands meghanada-mode
  :config
  (setq meghanada-server-install-dir (concat doom-etc-dir "meghanada-server/")
        meghanada-use-company (featurep! :completion company)
        meghanada-use-flycheck (featurep! :feature syntax-checker)
        meghanada-use-eldoc t
        meghanada-use-auto-start t)

  ;; Setup on first use
  (meghanada-install-server)
  (if (file-exists-p (meghanada--locate-server-jar))
      (add-hook! 'java-mode-hook #'(meghanada-mode flycheck-mode))
    (warn "java-mode: meghanada-server not installed, java-mode will run with reduced functionality"))

  (add-hook 'java-mode-hook #'(rainbow-delimiters-mode eldoc-mode))

  (set! :build 'compile-file    'java-mode #'meghanada-compile-file)
  (set! :build 'compile-project 'java-mode #'meghanada-compile-project)

  (set! :jump 'java-mode :definition #'meghanada-jump-declaration)

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

