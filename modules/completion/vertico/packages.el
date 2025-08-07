;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "b43b594bb318e722b908f01a6a11409af14b1413")

(package! orderless :pin "254f2412489bbbf62700f9d3d5f18e537841dcc3")

(package! consult :pin "7146596b564fb0a52b5bff420f27454911f603c8")
(package! consult-dir :pin "4532b8d215d16b0159691ce4dee693e72d71e0ff")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "8067363ee33c01d339d9f18091dce5f18e3b97ee"))
(package! embark :pin "2941f2ea36d61c1a84c3f79ebe47d604c9a92b5d")
(package! embark-consult :pin "2941f2ea36d61c1a84c3f79ebe47d604c9a92b5d")

(package! marginalia :pin "0e7097051cbcedcc4da9b633406291d4052ec0e4")

(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "e15e21a263bad06424982c11e8d68ffe1372a4e7"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "c5a8b5f72a582e88a2a696a3bbc2df7af28bd229"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
