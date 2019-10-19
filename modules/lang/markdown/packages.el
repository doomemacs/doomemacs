;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode)
(package! markdown-toc)
(package! edit-indirect)

(when (featurep! +grip)
  (package! grip-mode))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")))
