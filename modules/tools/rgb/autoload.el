;;; tools/rgb/autoload.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui hydra)

;;;###autoload (autoload '+rgb/kurecolor-hydra/body "tools/rgb/autoload" nil t)
(defhydra +rgb/kurecolor-hydra (:color pink :hint nil)
  "
Inc/Dec      _w_/_W_ brightness      _d_/_D_ saturation      _e_/_E_ hue    "
  ("w" kurecolor-decrease-brightness-by-step)
  ("W" kurecolor-increase-brightness-by-step)
  ("d" kurecolor-decrease-saturation-by-step)
  ("D" kurecolor-increase-saturation-by-step)
  ("e" kurecolor-decrease-hue-by-step)
  ("E" kurecolor-increase-hue-by-step)
  ("q" nil "cancel" :color blue))
