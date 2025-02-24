;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "c302b0526ad9d0c99d1d0639df63f6ca65accfc3")
(package! cape :pin "50fdb708074e91bc3d465fbc86c68cbda353b319")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "0a932e89cf88f300fa37f70944d73acc8bc0676c"))
(when (and (not (modulep! :completion vertico))
           (modulep! +orderless))
  ;; enabling +orderless without vertico should be fairly niche enough that to
  ;; save contributor headaches we should only pin vertico's orderless and leave
  ;; this one unpinned
  (package! orderless))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "4c2e33d70cd1d95cf1e08d134b058a6dd90a99c9"))
