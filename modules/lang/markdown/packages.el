;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "1f709778ac7990f4a07fdf11fe37bc6541810b29")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")
(package! edit-indirect :pin "f80f63822ffae78de38dbe72cacaeb1aaa96c732")

(when (modulep! +grip)
  (package! grip-mode :pin "6d6ddbe0af39c82a633add8499488ad8dc9e1daa"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
