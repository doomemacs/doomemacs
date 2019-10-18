;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode)
(package! markdown-toc)
(package! edit-indirect)

(when (featurep! +grip)
  (package! grip-mode))
