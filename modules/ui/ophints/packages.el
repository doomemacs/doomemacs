;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (modulep! :editor evil)
    (package! evil-goggles :pin "34ca276a85f615d2b45e714c9f8b5875bcb676f3")
  (package! goggles :pin "142d788e16bf387608b51c8590c852940890f637"))
