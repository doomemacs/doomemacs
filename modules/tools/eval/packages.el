;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun)
(when (featurep! +overlay)
  (package! eros))
