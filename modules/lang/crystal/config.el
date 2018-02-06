;;; lang/crystal/config.el -*- lexical-binding: t; -*-

(def-package! crystal-mode
  :mode "\\.cr$"
  :interpreter "crystal"
  :config
  (set! :eval 'crystal-mode
        '((:command     . "crystal")
          (:exec        . "%c %s")
          (:description . "Run Crystal script"))))


(def-package! flycheck-crystal
  :after crystal-mode
  :config (add-hook 'crystal-mode-hook #'flycheck-mode))


(def-package! inf-crystal
  :commands (inf-crystal crystal-switch-to-inf))
