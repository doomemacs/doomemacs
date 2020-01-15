;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "e9dff50d572caa96b68a7466c18c97a8d6ed651c")
(package! markdown-toc :pin "7038f4f6d5c2bc7e4aea89699a607ac2b7dd16a8")
(package! edit-indirect :pin "935ded353b9ed3da67bc61abf245c21b58d88864")

(when (featurep! +grip)
  (package! grip-mode :pin "0c2fe11f12ec23d5bbfba59ba43b89e87ef3eea8"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown") :pin "46cd81b37991c4325fc24015a610f832b0ff995d"))
