;;; lang/crystal/config.el -*- lexical-binding: t; -*-

(def-package! crystal-mode
  :defer t
  :config
  (set! :lookup 'crystal-mode
    :definition #'crystal-def-jump
    :references #'crystal-tool-imp)
  (set! :eval 'crystal-mode
        '((:command     . "crystal")
          (:exec        . "%c %s")
          (:description . "Run Crystal script"))))


(def-package! flycheck-crystal
  :when (featurep! :feature syntax-checker)
  :after crystal-mode
  :config (add-hook 'crystal-mode-hook #'flycheck-mode))


(def-package! inf-crystal :commands crystal-switch-to-inf)
