;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "0642f71071"))
(package! anzu :pin "2e69955da9")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "9bca6ca14d"))
