;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "ef2cb4d94af68908a4963afa492afba876725128")
(package! markdown-toc :pin "9565eeaa1d26bc0ab83eb65bd30470888f724044")
(package! edit-indirect :pin "bdc8f542fe8430ba55f9a24a7910639d4c434422")

(when (featurep! +grip)
  (package! grip-mode :pin "281ada2c93bac7043c6f665fac065a17d4247bdc"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "064fe9b4767470472356d20bdd08e2f30ebbc9ac"))
