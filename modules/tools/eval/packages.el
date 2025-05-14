;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "a5c9a5e8c674e0279b0bcec4dcac4f7583f1cfe9")
(when (modulep! +overlay)
  (package! eros :pin "a9a92bdc6be0521a6a06eb464be55ed61946639c"))
