;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (modulep! :editor evil)
    (package! evil-goggles :pin "0070c9d8447e1696f8713d0c13ff64ef0979d580")
  (package! volatile-highlights :pin "fcf6e2778454ce514c189a7d1fe70e03ad81c325"))
