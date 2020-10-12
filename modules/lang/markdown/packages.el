;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "152eae2415258141043e559af97d37a72de6c4e5")
(package! markdown-toc :pin "9565eeaa1d26bc0ab83eb65bd30470888f724044")
(package! edit-indirect :pin "bdc8f542fe8430ba55f9a24a7910639d4c434422")

(when (featurep! +grip)
  (package! grip-mode :pin "91da46f29c455c3cd24b2a6b20ff2db2c4ce8cd6"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "064fe9b4767470472356d20bdd08e2f30ebbc9ac"))
