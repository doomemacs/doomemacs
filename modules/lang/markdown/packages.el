;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "541bd7b48a4b7586f3c419f9ee1bb24810e1f56d")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")
(package! edit-indirect :pin "7fffd87ac3b027d10a26e8492629da01a4cd7633")

(when (featurep! +grip)
  (package! grip-mode :pin "9220a560b4ac8431067be9c25a4c7f19075dc525"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
