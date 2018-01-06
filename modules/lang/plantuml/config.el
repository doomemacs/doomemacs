;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(def-package! plantuml-mode
  :mode "\\.p\\(lant\\)?uml$"
  :config
  (setq plantuml-jar-path (concat doom-etc-dir "plantuml.jar"))
  (set! :popup "^\\*PLANTUML" '((size . 0.4)) '((select) (transient . 0)))

  (unless (executable-find "java")
    (warn "plantuml-mode: can't find java, preview disabled."))
  (unless (file-exists-p plantuml-jar-path)
    (warn "plantuml-mode: can't find plantuml.jar; run M-x +plantuml/install.")))


(def-package! flycheck-plantuml
  :when (featurep! :feature syntax-checker)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))
