;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "862ae8addd29bf6affca1a35fd0176cb0c1392da")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")
(package! edit-indirect :pin "bdc8f542fe8430ba55f9a24a7910639d4c434422")

(when (featurep! +grip)
  (package! grip-mode :pin "1c82e27beec629514a8039e22f4f7c649e77ee2b"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
