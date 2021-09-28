;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "e92cb37457b43336b765630dbfbea8ba4be601fa")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "d3c5bacc9c697c4cf8b14616c4199210f9267068")

(unless (featurep! :editor evil)
  (package! expand-region :pin "95a773bd8f557cbd43d3b2dab2fa4417ec5927ab"))
