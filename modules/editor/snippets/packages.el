;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "3bf9a3b1af37174a004798b7195826af0123fa6a")
(package! auto-yasnippet :pin "db9e0dd4335b2202cd5dac95bbbc87a1032d9bbe")

(package! doom-snippets
  :recipe (:host github
           :repo "hlissner/doom-snippets"
           :files ("*.el" "*"))
  :pin "30a78a2da2b514e8da15b4c5df2df48356cfe4d8")
