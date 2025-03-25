;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "4b6df453f80ae7eab250a312056eaef95890e4c6")
(when (modulep! +overlay)
  (package! eros :pin "a9a92bdc6be0521a6a06eb464be55ed61946639c"))
