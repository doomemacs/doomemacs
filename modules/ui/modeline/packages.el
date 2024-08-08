;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (modulep! +light)
  (package! doom-modeline :pin "9920ef511620e9fa5599cb357e48487f758b1bb1"))
(package! anzu :pin "26fb50b429ee968eb944b0615dd0aed1dd66172c")
(when (modulep! :editor evil)
  (package! evil-anzu :pin "d1e98ee6976437164627542909a25c6946497899"))
