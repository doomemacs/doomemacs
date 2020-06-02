;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "c6ce1f315b768af8688d06bc57d2eb403f875a63")
(when (featurep! +overlay)
  (package! eros :pin "dd8910279226259e100dab798b073a52f9b4233a"))
