;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "50e07e7698")
(when (featurep! +overlay)
  (package! eros :pin "dd89102792"))
