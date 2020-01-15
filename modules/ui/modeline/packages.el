;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "dbd9251979de2d94073197b406fe5a72aa593e5d"))
(package! anzu :pin "592f8ee6d0b1bc543943b36a30063c2d1aac4b22")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "9bca6ca14d865e7e005bc02a28a09b4ae74facc9"))
