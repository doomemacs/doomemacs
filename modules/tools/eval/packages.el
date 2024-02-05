;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "248149b0261051bd9eec8bdbc21c22d18d7b1b11")
(when (modulep! +overlay)
  (package! eros :pin "a9a92bdc6be0521a6a06eb464be55ed61946639c"))
