;;; lang/scala/config.el -*- lexical-binding: t; -*-

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (add-hook 'scala-mode-hook #'ensime-mode)
  (setq scala-indent:align-parameters t)
  (set! :company-backend 'scala-mode '(ensime-company company-yasnippet)))


(def-package! sbt-mode :after scala-mode)


(def-package! ensime
  :commands (ensime ensime-mode ensime-scala-mode-hook)
  :config
  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints t)

  (add-hook 'ensime-mode-hook #'eldoc-mode))

