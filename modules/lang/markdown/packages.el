;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "051734091aba17a54af96b81beebdbfc84c26459")
(package! markdown-toc :pin "9565eeaa1d26bc0ab83eb65bd30470888f724044")
(package! edit-indirect :pin "bdc8f542fe8430ba55f9a24a7910639d4c434422")

(when (featurep! +grip)
  (package! grip-mode :pin "98d566db769b865f102a0c6802a08ebce8ae5e7f"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "064fe9b4767470472356d20bdd08e2f30ebbc9ac"))
