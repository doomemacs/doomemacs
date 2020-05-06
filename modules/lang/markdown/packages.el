;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "c927a114b1b23cf7538181d62fd14679cce7fa25")
(package! markdown-toc :pin "eda9650a1bf0015e52e9678bd92b0a8beb1d7d71")
(package! edit-indirect :pin "935ded353b9ed3da67bc61abf245c21b58d88864")

(when (featurep! +grip)
  (package! grip-mode :pin "9615c4774727a719d38313a679d70f2a2c6aca68"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "46cd81b37991c4325fc24015a610f832b0ff995d"))
