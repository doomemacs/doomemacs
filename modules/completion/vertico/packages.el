;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "93f15873d7d6244d72202c5dd7724a030a2d5b9a")

(package! orderless :pin "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7")

(package! consult :pin "d1d39d52151a10f7ca29aa291886e99534cc94db")
(package! consult-dir :pin "1497b46d6f48da2d884296a1297e5ace1e050eb5")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "9fe96c4b75c8566170ad41a04c3849d2e2606104"))
(package! embark :pin "7b3b2fa239c34c2e304eab4367a4f5924c047e2b")
(package! embark-consult :pin "7b3b2fa239c34c2e304eab4367a4f5924c047e2b")

(package! marginalia :pin "0d08fbea0f1182627891240780081ba528c1348b")

(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "d09ea987ed3d2cc64137234f27851594050e2b64"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "d6e06a4f1b34d24cc0ca6ec69d2d6c965191b23e"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
