;;; module-plantuml.el

(use-package puml-mode
  :mode "\\.p\\(lant\\)?uml$"
  :init
  (setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8029/plantuml.8029.jar"))

(provide 'module-plantuml)
;;; module-plantuml.el ends here
