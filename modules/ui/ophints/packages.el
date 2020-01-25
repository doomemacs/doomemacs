;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (featurep! :editor evil)
    (package! evil-goggles :pin "08a22058fd")
  (package! volatile-highlights :pin "9a20091f0c"))
