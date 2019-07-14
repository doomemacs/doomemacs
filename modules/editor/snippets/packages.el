;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet)
(package! auto-yasnippet)

(package! doom-snippets
  :recipe (:fetcher github
           :repo "hlissner/doom-snippets"
           :files ("*.el" "snippets")))
