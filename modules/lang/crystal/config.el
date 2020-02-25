;;; lang/crystal/config.el -*- lexical-binding: t; -*-

(after! crystal-mode
  (set-lookup-handlers! 'crystal-mode
    :definition #'crystal-def-jump
    :references #'crystal-tool-imp)
  (set-eval-handler! 'crystal-mode
    '((:command     . "crystal")
      (:exec        . "%c %s")
      (:description . "Run Crystal script"))))


(use-package! flycheck-crystal
  :when (featurep! :checkers syntax)
  :after crystal-mode)


(use-package! flycheck-ameba
  :when (featurep! :checkers syntax)
  :after crystal-mode
  :config (flycheck-ameba-setup))


(use-package! inf-crystal
  :commands crystal-switch-to-inf)
