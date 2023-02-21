;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (modulep! :editor evil)
    (package! evil-goggles :pin "8f20a16e74016f37ad76dc4f2230d9b00c6df3c2")
  (package! volatile-highlights :pin "513c8b73cd3bc06cb9936a100468c227f649851c"))
