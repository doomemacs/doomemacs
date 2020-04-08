;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "b44955841a"))
(package! anzu :pin "3e34fb3df5")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "9bca6ca14d"))
