;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "314beae43cac2e4943e9ed4850e8e147bc3d2fac")
(when (modulep! +overlay)
  (package! eros :pin "dd8910279226259e100dab798b073a52f9b4233a"))
