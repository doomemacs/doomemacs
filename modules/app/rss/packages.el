;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "55fb162fa27e71b88effa59a83c57842e262b00f")
(package! elfeed-goodies :pin "544ef42ead011d960a0ad1c1d34df5d222461a6b")
(when (modulep! +org)
  (package! elfeed-org :pin "3242ec0519800a58f20480c8a6e3b3337d137084"))
