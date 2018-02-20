;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode)
(package! alchemist :recipe
  (:fetcher github
   :repo "hlissner/alchemist.el"
   :files (:defaults "*.exs" "alchemist-server")))
(package! ac-alchemist)
