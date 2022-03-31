;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "c680f5137c49e352ff3779f9c82a3f1bd4a2d9b6")
(when (featurep! +overlay)
  (package! eros :pin "dd8910279226259e100dab798b073a52f9b4233a"))
