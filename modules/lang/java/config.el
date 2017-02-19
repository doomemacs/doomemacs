;;; lang/java/config.el

(@def-package meghanada
  :commands meghanada-mode
  :init
  (@add-hook java-mode '(meghanada-mode flycheck-mode))
  :config
  (@set :build 'compile-file    'java-mode 'meghanada-compile-file)
  (@set :build 'compile-project 'java-mode 'meghanada-compile-project)

  (setq meghanada-server-install-dir (expand-file-name "meghanada-server" doom-cache-dir)
        meghanada-use-company t
        meghanada-use-flycheck t
        meghanada-use-auto-start t)

  (@map :map meghanada-mode-map :m "gd" 'meghanada-jump-declaration)

  (unless (file-exists-p (meghanada--locate-server-jar))
    (meghanada-install-server)))


(@def-package android-mode
  :commands android-mode
  :init
  (@add-hook (java-mode groovy-mode nxml-mode) '+java|android-mode-maybe)
  :config
  (@set :yas-minor-mode 'android-mode)
  (@set :company-dict-minor-mode 'android-mode))


(@def-package groovy-mode
  :mode "\\.g\\(radle\\|roovy\\)$"
  :config
  (@set :eval 'groovy-mode "groovy"))

