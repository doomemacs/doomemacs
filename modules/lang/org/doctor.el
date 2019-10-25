;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/org/doctor.el

(when (featurep! +gnuplot)
  (unless (executable-find "gnuplot")
    (warn! "Couldn't find gnuplot. org-plot/gnuplot will not work")))
