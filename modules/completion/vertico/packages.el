;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "e0bb699ebf9e41893dbd19e7d19381fa73c08bc1")

(package! orderless :pin "254f2412489bbbf62700f9d3d5f18e537841dcc3")

(package! consult :pin "f94d557807451a733b71a2c41d43175ec6ed3248")
(package! consult-dir :pin "4532b8d215d16b0159691ce4dee693e72d71e0ff")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "3bc2141daf8cfad7e4d2e2f78b15d45033f707a5"))
(package! embark :pin "d5df0eff182b014ab49328a4dbb1d69eb7faafbd")
(package! embark-consult :pin "d5df0eff182b014ab49328a4dbb1d69eb7faafbd")

(package! marginalia :pin "c51fd9e4d4258543e0cd8dedda941789163bec5a")

(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "8e5b995eb2439850ab21ba6062d9e6942c82ab9c"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "c5a8b5f72a582e88a2a696a3bbc2df7af28bd229"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "834d39acfe8a7d2c304afbe4d649b9372118c756"))
