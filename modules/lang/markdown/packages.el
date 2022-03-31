;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "521658eb32e456681592443e04ae507c3a59ed07")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")
(package! edit-indirect :pin "e3d86416bcf8ddca951d7d112e57ad30c5f9a081")

(when (featurep! +grip)
  (package! grip-mode :pin "6b427143a8f61bb0b5dd070d554e5058130d15ff"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
