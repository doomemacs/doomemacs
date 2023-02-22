;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "c765b73b370f0fcaaa3cee28b2be69652e2d2c39")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")
(package! edit-indirect :pin "f80f63822ffae78de38dbe72cacaeb1aaa96c732")

(when (modulep! +grip)
  (package! grip-mode :pin "5809fb62f6dd7b4bfa7685203aaa1474fca70f4e"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
