;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "162d7d545ed41c27967d108c04aa31f5a61c8e16")
(package! elfeed-goodies :pin "544ef42ead011d960a0ad1c1d34df5d222461a6b")
(when (modulep! +org)
  (package! elfeed-org :pin "3242ec0519800a58f20480c8a6e3b3337d137084"))
