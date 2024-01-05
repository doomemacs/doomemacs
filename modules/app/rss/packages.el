;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "55fb162fa27e71b88effa59a83c57842e262b00f")
(package! elfeed-goodies :pin "544ef42ead011d960a0ad1c1d34df5d222461a6b")
(package! elfeed-tube :pin "0291038a00c17d780aded05b1e90860a2d586546")
(when (modulep! +org)
  (package! elfeed-org :pin "fe59a96969bd321f5f9ec7317a4bc80943b94c86"))
