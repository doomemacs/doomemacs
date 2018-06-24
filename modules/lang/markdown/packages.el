;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode)
(package! markdown-toc)

(when (featurep! +pandoc)
  (package! pandoc-mode))


