;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "c1e7b6190b00158e67347b4db0a8f7964e5d2f8b")
(package! cape :pin "a397a0c92de38277b7f835fa999fac400a764908")
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
  (package! yasnippet-capf :pin "9043f8275176a8f198ce8e81fadab1870fa165bb"))
