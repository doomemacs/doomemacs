;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "0a616caedf10ebba812a87de3adacd24cd46522b")
(package! cape :pin "9110956a5155d5e3c460160fa1b4dac59322c229")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "7077bb76fefc15aed967476406a19dc5c2500b3c"))
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
