;;; lang/crystal/config.el -*- lexical-binding: t; -*-

(after! crystal-mode
  (set-lookup-handlers! 'crystal-mode
    :definition #'crystal-def-jump
    :references #'crystal-tool-imp)
  (set-eval-handler! 'crystal-mode
    '((:command     . "crystal")
      (:exec        . "%c %s")
      (:description . "Run Crystal script")))
  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(crystal-mode ruby crystal-indent-level))))


(def-package! flycheck-crystal
  :when (featurep! :feature syntax-checker)
  :after crystal-mode)


(def-package! inf-crystal
  :commands crystal-switch-to-inf)
