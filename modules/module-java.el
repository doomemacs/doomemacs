;;; module-java.el

(use-package meghanada
  :commands meghanada-mode
  :init
  (add-hook! java-mode '(meghanada-mode flycheck-mode))
  :config
  (def-docset! java-mode "java,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc")
  (def-builder! java-mode meghanada-compile-project)

  (setq meghanada-server-install-dir (f-expand "meghanada-server" doom-ext-dir)
        meghanada-use-company t
        meghanada-use-flycheck t
        meghanada-use-auto-start t)

  (unless (f-exists? (meghanada--locate-server-jar))
    (meghanada-install-server))

  (map! :map meghanada-mode-map :m "gd" 'meghanada-jump-declaration))

(use-package android-mode
  :commands android-mode
  :init
  (add-hook! (java-mode groovy-mode nxml-mode) 'doom|android-mode-enable-maybe)
  :config
  (def-yas-mode! android-mode)
  (after! company-dict
    (push 'android-mode company-dict-minor-mode-list)))

(use-package groovy-mode
  :mode "\\.g\\(radle\\|vy\\|roovy\\)$"
  :config
  (after! quickrun
    (push '("\\.gvy$" . "groovy") quickrun-file-alist)))

(provide 'module-java)
;;; module-java.el ends here
