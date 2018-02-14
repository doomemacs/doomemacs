;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(def-package! plantuml-mode
  :mode "\\.p\\(lant\\)?uml$"
  :init
  (setq plantuml-jar-path (concat doom-etc-dir "plantuml.jar"))
  :config
  (set! :popup "^\\*PLANTUML" '((size . 0.4)) '((select) (transient . 0)))

  (unless (executable-find "java")
    (warn! "Couldn't find java. Disabling plantuml preview."))
  (unless (file-exists-p plantuml-jar-path)
    (warn! "Couldn't find plantuml.jar. Install it witInstall it with-x +plantuml/install.")))


(def-package! flycheck-plantuml
  :when (featurep! :feature syntax-checker)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))
