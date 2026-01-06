;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun :pin "9199e222f95104ee83e115a9d5ac159d86816706")
(when (modulep! +overlay)
  (package! eros :pin "66ee90baa3162fea028f5101ddcc370f7d1d4fcf"))
