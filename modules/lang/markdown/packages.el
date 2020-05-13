;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "f47a2e9796dfdde6fae7920af23647fe027dc34e")
(package! markdown-toc :pin "a9f13eecd0c7d8be960055dbc2d6f5d3fe6f40ca")
(package! edit-indirect :pin "935ded353b9ed3da67bc61abf245c21b58d88864")

(when (featurep! +grip)
  (package! grip-mode :pin "9615c4774727a719d38313a679d70f2a2c6aca68"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "685d7fbb81bc02fa32779d2a127b99a0c8c7436b"))
