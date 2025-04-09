;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "061d926d0f0eb2633416deeddc403a1a67b062ae")
(package! cape :pin "f72ebcaeff4252ca0d7a9ac4636d8db0fdf54c55")
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
