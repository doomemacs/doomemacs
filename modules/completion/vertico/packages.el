;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "c682ef50e62237435e9fc287927ce4181b49be90")

(package! orderless :pin "49d1fdfb80b55699a00b11bc916ad29c0447039b")

(package! consult :pin "0c3f53916ea0db0c472c0a0c620a85cc1b00caf2")
(package! consult-dir :pin "15891383f34d43acc5bb82bda92239b1f54cf178")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "3b999ae983900c16c0b5b5c30b7eca640d386a76"))
(package! embark :pin "19a13e344e04bbf861eaa74491b23da52b398672")
(package! embark-consult :pin "19a13e344e04bbf861eaa74491b23da52b398672")

(package! marginalia :pin "50a51c69f006ec8b3ba1c570555d279d4cff6d99")

(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "426a1d7c29a04ae8e6ae9b55b0559f11a1e8b420"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "2e0e09e5bbd6ec576ddbe566ab122575ef051fab"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "834d39acfe8a7d2c304afbe4d649b9372118c756"))
