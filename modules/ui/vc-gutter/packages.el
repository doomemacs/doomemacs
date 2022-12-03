;; -*- no-byte-compile: t; -*-
;;; ui/vc-gutter/packages.el

(if (modulep! +diff-hl)
    (package! diff-hl :pin "dabb7be6283488abd8d232ea8ce590d502713ed8")
  (package! git-gutter-fringe :pin "648cb5b57faec55711803cdc9434e55a733c3eba"))
