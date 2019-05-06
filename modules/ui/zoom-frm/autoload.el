;;; ui/zoom-frm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'doom-frame-zoom-hydra/body "ui/zoom-frm/autoload.el" nil t)
(defhydra doom-frame-zoom-hydra (:hint t :color red)
  "
      Frame zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
  ("j" zoom-frm-in "in")
  ("k" zoom-frm-out "out")
  ("0" zoom-frm-unzoom "reset"))
