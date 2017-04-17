;;; lang/scala/config.el

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (add-hook 'scala-mode-hook #'eldoc-mode)
  :config
  (set! :company-backend 'scala-mode '(ensime-company (company-yasnippet))))


(def-package! sbt-mode :after scala-mode)


(def-package! ensime
  :commands (ensime ensime-mode ensime-scala-mode-hook)
  :init
  (add-hook 'scala-mode-hook #'ensime-scala-mode-hook))

