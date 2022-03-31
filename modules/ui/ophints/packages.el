;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (featurep! :editor evil)
    (package! evil-goggles :pin "8f20a16e74016f37ad76dc4f2230d9b00c6df3c2")
  (package! volatile-highlights :pin "9a20091f0ce7fc0a6b3e641a6a46d5f3ac4d8392"))
