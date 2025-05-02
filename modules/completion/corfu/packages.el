;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "2e05fe8244fff22c3c3d2af4334b1850250212a9")
(package! cape :pin "5546a2db8e3928d5a95e1174e69a5742ddf97c0f")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "55b17ee20a5011c6a9be8beed6a9daf644815b5a"))
(when (and (not (modulep! :completion vertico))
           (modulep! +orderless))
  ;; Enabling +orderless without vertico should be fairly niche enough that to
  ;; save contributor headaches we should only pin vertico's orderless and leave
  ;; this one unpinned.
  (package! orderless))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "de6446732b106965ea583c9e076770694f7226b8"))
