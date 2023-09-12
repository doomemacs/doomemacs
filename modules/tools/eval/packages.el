;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "6f963189305e8311c8193ba774f4244eb1315f57")
(when (modulep! +overlay)
  (package! eros :pin "a9a92bdc6be0521a6a06eb464be55ed61946639c"))
