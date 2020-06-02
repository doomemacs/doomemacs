;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "770e3aa7cdfc9d731119b9425e8a7c8ac6dd5f93")
(package! markdown-toc :pin "9565eeaa1d26bc0ab83eb65bd30470888f724044")
(package! edit-indirect :pin "935ded353b9ed3da67bc61abf245c21b58d88864")

(when (featurep! +grip)
  (package! grip-mode :pin "9615c4774727a719d38313a679d70f2a2c6aca68"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "064fe9b4767470472356d20bdd08e2f30ebbc9ac"))
