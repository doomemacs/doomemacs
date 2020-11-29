;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "fd96577486f043da8c930aa0b84026f63f37d682")
(when (featurep! +overlay)
  (package! eros :pin "dd8910279226259e100dab798b073a52f9b4233a"))
