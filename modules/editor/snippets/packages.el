;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "5b1217ab085fab4abeb1118dccb260691b446703")
(package! auto-yasnippet :pin "db9e0dd4335b2202cd5dac95bbbc87a1032d9bbe")
(package! doom-snippets
  :recipe (:host github
           :repo "hlissner/doom-snippets"
           :files ("*.el" "*"))
  :pin "422f683adfbec1b01fe00524690b64dc9e702ae0")
