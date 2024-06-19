;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "cdc3e13ad312f5f12b3f78f842fff0b398eb4473")
(package! cape :pin "f61da109a9e4491614938c300291060fd8855c1b")
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
  (package! yasnippet-capf :pin "744dedb7837d0c7e07817d36ec752a0cd813f55c"))
