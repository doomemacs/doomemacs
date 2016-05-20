;;; module-scala.el

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :init (add-hook 'scala-mode-hook 'turn-on-eldoc-mode)
  :config
  (def-company-backend! scala-mode '(ensime-company (company-yasnippet)))
  (def-docset! scala-mode "scala"))

(use-package sbt-mode
  :after scala-mode)

(use-package ensime
  :commands (ensime ensime-mode ensime-scala-mode-hook)
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(provide 'module-scala)
;;; module-scala.el ends here
