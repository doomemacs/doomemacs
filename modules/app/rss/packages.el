;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "e29c8b91450bd42d90041231f769c4e5fe5070da")
(when (featurep! +org)
  (package! elfeed-org :pin "268efdd0121fa61f63b722c30e0951c5d31224a4"))
