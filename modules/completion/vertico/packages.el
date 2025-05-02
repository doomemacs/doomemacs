;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "304be874be3d6198f80f987c1b433c816c4e1d3f")

(package! orderless :pin "254f2412489bbbf62700f9d3d5f18e537841dcc3")

(package! consult :pin "ee64a2a2998e7ae462f5125f280cd656c18c77b4")
(package! consult-dir :pin "4532b8d215d16b0159691ce4dee693e72d71e0ff")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "77d3e790a322934ecb63ac0e8056b7a7b3d39fdf"))
(package! embark :pin "923d0ec52e2e3e0ae44e497c31c7888e87d08a8f")
(package! embark-consult :pin "923d0ec52e2e3e0ae44e497c31c7888e87d08a8f")

(package! marginalia :pin "2ff4d690f78fb86573c11a32631e53627947ebee")

(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "8e5b995eb2439850ab21ba6062d9e6942c82ab9c"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "c5a8b5f72a582e88a2a696a3bbc2df7af28bd229"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
