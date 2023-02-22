;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "7a89313c07a21eae9cd69a1a98e2a134d559e04f")
(when (modulep! +overlay)
  (package! eros :pin "dd8910279226259e100dab798b073a52f9b4233a"))
