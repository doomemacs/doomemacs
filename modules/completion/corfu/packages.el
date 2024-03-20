;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "c1e7b6190b00158e67347b4db0a8f7964e5d2f8b")
(package! cape :pin "a397a0c92de38277b7f835fa999fac400a764908")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "7077bb76fefc15aed967476406a19dc5c2500b3c"))
(when (modulep! +orderless)
  (package! orderless :pin "b24748093b00b37c3a572c4909f61c08fa27504f"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "9043f8275176a8f198ce8e81fadab1870fa165bb"))
