;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "fa9fa20e3236006c2cf278209356f60cc4175120")
(package! markdown-toc :pin "9565eeaa1d26bc0ab83eb65bd30470888f724044")
(package! edit-indirect :pin "935ded353b9ed3da67bc61abf245c21b58d88864")

(when (featurep! +grip)
  (package! grip-mode :pin "52768a0187f8ce9b42010dc45bbc432551aeccee"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "064fe9b4767470472356d20bdd08e2f30ebbc9ac"))
