;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "57db985c6d293747dc8c40c4e08b465e148613a8")
(when (featurep! +overlay)
  (package! eros :pin "dd8910279226259e100dab798b073a52f9b4233a"))
