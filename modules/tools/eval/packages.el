;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "7345432cea36d4a6cdfcf5371f76af640cc6d192")
(when (modulep! +overlay)
  (package! eros :pin "a9a92bdc6be0521a6a06eb464be55ed61946639c"))
