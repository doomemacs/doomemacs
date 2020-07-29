;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/org/doctor.el

(when (featurep! +gnuplot)
  (unless (executable-find "gnuplot")
    (warn! "Couldn't find gnuplot. org-plot/gnuplot will not work")))

(when (featurep! +roam)
  (unless (executable-find "sqlite3")
    (warn! "Couldn't find the sqlite3 executable. org-roam will not work."))
  (unless (executable-find "dot")
    (warn! "Couldn't find the dot executable (from graphviz). org-roam will not be able to generate graph visuallizations.")))
