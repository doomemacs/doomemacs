;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "53aa6c85be72ce220a4321487c535295b0de0488")
(package! cape :pin "c9191ee9e13e86a7b40c3d25c8bf7907c085a1cf")
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
  (package! yasnippet-capf :pin "f53c42a996b86fc95b96bdc2deeb58581f48c666"))
