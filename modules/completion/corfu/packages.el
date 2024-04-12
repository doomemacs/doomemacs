;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "35cd5a0f3cba89766072e3e933d1fe2ee02f2289")
(package! cape :pin "e01e4430234850263d326ad4521849cd46e64059")
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
