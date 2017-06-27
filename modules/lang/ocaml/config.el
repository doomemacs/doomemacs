;;; lang/ocaml/config.el

(def-package! tuareg
  :mode (("\\.ml$"  . tuareg-mode)
         ("\\.mll$" . tuareg-mode)
         ("\\.mly$" . tuareg-mode))
  :config
)

(def-package! merlin
  :after tuareg
  :config
  (add-hook! 'tuareg-mode-hook #'merlin-mode)
)
