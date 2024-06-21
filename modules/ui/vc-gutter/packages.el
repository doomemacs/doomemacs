;; -*- no-byte-compile: t; -*-
;;; ui/vc-gutter/packages.el

(if (modulep! +diff-hl)
    (package! diff-hl :pin "11f3113e790526d5ee00f61f8e7cd0d01e323b2e")
  (package! git-gutter-fringe :pin "648cb5b57faec55711803cdc9434e55a733c3eba"))
