;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "b1a862f0165b7bafe0f874738a55be1b1720dd7d")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")
(package! edit-indirect :pin "f80f63822ffae78de38dbe72cacaeb1aaa96c732")

(when (modulep! +grip)
  (package! grip-mode :pin "5809fb62f6dd7b4bfa7685203aaa1474fca70f4e"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
