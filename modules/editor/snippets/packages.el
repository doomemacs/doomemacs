;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "5cbdbf0d2015540c59ed8ee0fcf4788effdf75b6")
(package! auto-yasnippet :pin "db9e0dd4335b2202cd5dac95bbbc87a1032d9bbe")
(package! doom-snippets
  :recipe (:host github
           :repo "hlissner/doom-snippets"
           :files ("*.el" "*"))
  :pin "f7747da6343aadfbe8a3f9e6b35018ac19db4438")
