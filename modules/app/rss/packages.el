;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "5c05a1eab37bc113ecb158a4d57fe05352fa2c6a")
(package! elfeed-goodies :pin "544ef42ead011d960a0ad1c1d34df5d222461a6b")
(when (modulep! +org)
  (package! elfeed-org :pin "d62d23e25c5e3be3d70b7fbe1eaeb6e43f93a061"))
(when (modulep! +youtube)
  (package! elfeed-tube :pin "0c3fbc21259e1fa794f3179a53b410ba610231f2"))
