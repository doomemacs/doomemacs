;; -*- no-byte-compile: t; -*-
;;; ui/vc-gutter/packages.el

(if (modulep! +diff-hl)
    (package! diff-hl :pin "b8b2727a72fdf64ac98e6cfa136a43cb0cacf72f")
  (package! git-gutter-fringe :pin "648cb5b57faec55711803cdc9434e55a733c3eba"))
