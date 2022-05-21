;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "0ccd59aaace34546017a1a0d7c393749747d5bc6")
(package! elfeed-goodies :pin "6711de66c22360f80fcfd9730293e5f3d419f787")
(when (featurep! +org)
  (package! elfeed-org :pin "e6bf4268485703907a97896fb1080f59977c9e3d"))
