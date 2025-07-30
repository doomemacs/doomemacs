;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (modulep! :editor evil)
    (package! evil-goggles :pin "34ca276a85f615d2b45e714c9f8b5875bcb676f3")
  (package! goggles :pin "5176114e99d4c13f57777abbcbcea0dbee9e4ca3"))
