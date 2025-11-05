;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (modulep! :editor evil)
    (package! evil-goggles :pin "34ca276a85f615d2b45e714c9f8b5875bcb676f3")
  (package! goggles :pin "6f87a700137c838568966bc8099dc15786897c32"))
