;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "6b1ceef882e9809083e660c10d669d132746b975")
(package! cape :pin "97641dcd1ebca1007badd26b2fb9269b86934c22")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "f821e953b1a3dc9b381bc53486aabf366bf11cb1"))
(when (and (not (modulep! :completion vertico))
           (modulep! +orderless))
  ;; Enabling +orderless without vertico should be fairly niche enough that to
  ;; save contributor headaches we should only pin vertico's orderless and leave
  ;; this one unpinned.
  (package! orderless))
(when (and (modulep! :os tty)
           (not (featurep 'tty-child-frames)))
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "f53c42a996b86fc95b96bdc2deeb58581f48c666"))
