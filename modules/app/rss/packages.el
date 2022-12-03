;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "162d7d545ed41c27967d108c04aa31f5a61c8e16")
(package! elfeed-goodies :pin "c9d9cd196746add3010d74f43b5c9866562f39fb")
(when (modulep! +org)
  (package! elfeed-org :pin "d28c858303e60dcb3a6eb18ea85ee3cb9e3dd623"))
