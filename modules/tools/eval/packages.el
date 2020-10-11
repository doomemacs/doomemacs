;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "005d269ce5080a127b44e0d29efe2f2e9852d0cc")
(when (featurep! +overlay)
  (package! eros :pin "dd8910279226259e100dab798b073a52f9b4233a"))
