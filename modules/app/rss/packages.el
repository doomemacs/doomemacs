;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "55fb162fa27e71b88effa59a83c57842e262b00f")
(package! elfeed-goodies :pin "544ef42ead011d960a0ad1c1d34df5d222461a6b")
(when (modulep! +org)
  (package! elfeed-org :pin "d62d23e25c5e3be3d70b7fbe1eaeb6e43f93a061"))
