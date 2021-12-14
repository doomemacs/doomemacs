;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "e92cb37457b43336b765630dbfbea8ba4be601fa")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "3be270f3a732dc4acae6a20ff449eef0c4f9a966")

(unless (featurep! :editor evil)
  (package! expand-region :pin "95a773bd8f557cbd43d3b2dab2fa4417ec5927ab"))
