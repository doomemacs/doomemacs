;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "63a43e13805ca3bc06f9492323d66e238c5d0fea")

(package! orderless :pin "26a384894678a1e51e3bf914af3699a61794fb57")

(package! consult :pin "d0370320d9fdde5ac6e0a27720f51138315af882")
(package! consult-dir :pin "1497b46d6f48da2d884296a1297e5ace1e050eb5")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "062e223bc6cf5f2126d7a107a35069c33c018c36"))
(package! embark :pin "7b3b2fa239c34c2e304eab4367a4f5924c047e2b")
(package! embark-consult :pin "7b3b2fa239c34c2e304eab4367a4f5924c047e2b")

(package! marginalia :pin "d38041df2c2d175040bbdb1df6e4cc75a75ca4f8")

(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "d09ea987ed3d2cc64137234f27851594050e2b64"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "d89a70743cfd95b7fcda621679b7555bbef51acb"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
