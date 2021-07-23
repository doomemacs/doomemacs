;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "243add9e74003cd5718f33482b7bb8b4fe140fb5")
(when (featurep! +org)
  (package! elfeed-org :pin "268efdd0121fa61f63b722c30e0951c5d31224a4"))
