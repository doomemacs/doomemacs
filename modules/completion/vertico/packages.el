;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "a7365958439c86f5cabb9cd4bb93c0a4118eedc1")

(package! orderless :pin "fb338f771f1693436da0472a8a4d230b28af14f3")

(package! consult :pin "4ca98db03dac33476d60657ba975639ee30f2e9e")
(package! consult-dir :pin "1497b46d6f48da2d884296a1297e5ace1e050eb5")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "e3fca5fadfa86cde5b1f2a5fc7c7669fb3423d15"))
(package! embark :pin "7b3b2fa239c34c2e304eab4367a4f5924c047e2b")
(package! embark-consult :pin "7b3b2fa239c34c2e304eab4367a4f5924c047e2b")

(package! marginalia :pin "fc0cee1151fced42db6014e1d29a61ed63de81d9")

(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "d09ea987ed3d2cc64137234f27851594050e2b64"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "d89a70743cfd95b7fcda621679b7555bbef51acb"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
