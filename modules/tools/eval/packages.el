;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "55bbe5d54b")
(when (featurep! +overlay)
  (package! eros :pin "dd89102792"))
