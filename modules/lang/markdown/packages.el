;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "e9dff50d57")
(package! markdown-toc :pin "7038f4f6d5")
(package! edit-indirect :pin "935ded353b")

(when (featurep! +grip)
  (package! grip-mode :pin "cbf20fd131"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown") :pin "46cd81b379"))
