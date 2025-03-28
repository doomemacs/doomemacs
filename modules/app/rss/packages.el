;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "a39fb78e34ee25dc8baea83376f929d7c128344f")
(package! elfeed-goodies :pin "544ef42ead011d960a0ad1c1d34df5d222461a6b")
(when (modulep! +org)
  (package! elfeed-org :pin "34c0b4d758942822e01a5dbe66b236e49a960583"))
(when (modulep! +youtube)
  (package! elfeed-tube :pin "79d5a08d76ea3ae96d7def9a5e2ede2e3562462a"))
