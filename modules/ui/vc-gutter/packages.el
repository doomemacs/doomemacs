;; -*- no-byte-compile: t; -*-
;;; ui/vc-gutter/packages.el

(if (modulep! +diff-hl)
    (package! diff-hl :pin "96620839430c1205cbb8c92dd54973397f70f9d2")
  (package! git-gutter-fringe :pin "648cb5b57faec55711803cdc9434e55a733c3eba"))
