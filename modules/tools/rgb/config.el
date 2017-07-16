;;; tools/rgb/config.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;

(def-package! rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

(def-package! kurecolor
  :after rainbow-mode
  :config
  (when (featurep! :feature hydra)
    (require 'hydra)
    (defhydra hydra-kurecolor (:color pink
                                      :hint  nil)
      "
Inc/Dec      _w_/_W_ brightness      _d_/_D_ saturation      _e_/_E_ hue    "
      ("w"  kurecolor-decrease-brightness-by-step)
      ("W"  kurecolor-increase-brightness-by-step)
      ("d"  kurecolor-decrease-saturation-by-step)
      ("D"  kurecolor-increase-saturation-by-step)
      ("e"  kurecolor-decrease-hue-by-step)
      ("E"  kurecolor-increase-hue-by-step)
      ("q" nil "cancel" :color blue))))
