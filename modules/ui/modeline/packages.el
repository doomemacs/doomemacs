;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (modulep! +light)
  (package! doom-modeline :pin "1505c13564b83e44d3187611e326a48b742cad3a"))
(package! anzu :pin "26fb50b429ee968eb944b0615dd0aed1dd66172c")
(when (modulep! :editor evil)
  (package! evil-anzu :pin "d1e98ee6976437164627542909a25c6946497899"))
