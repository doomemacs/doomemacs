;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "162d7d545ed41c27967d108c04aa31f5a61c8e16")
(package! elfeed-goodies :pin "8e4c1fbfb86226d867b524fd0f8ae78b4b602f1b")
(when (featurep! +org)
  (package! elfeed-org :pin "268efdd0121fa61f63b722c30e0951c5d31224a4"))
