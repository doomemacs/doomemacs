;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "c302b0526ad9d0c99d1d0639df63f6ca65accfc3")
(package! cape :pin "2e86b6deed2844fc1345ff01bc92c3a849a33778")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "13166345b290d6c6a2ac6ba94a8d28ec3bb58c67"))
(when (and (not (modulep! :completion vertico))
           (modulep! +orderless))
  ;; enabling +orderless without vertico should be fairly niche enough that to
  ;; save contributor headaches we should only pin vertico's orderless and leave
  ;; this one unpinned
  (package! orderless))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "de6446732b106965ea583c9e076770694f7226b8"))
