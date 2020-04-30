;; -*- no-byte-compile: t; -*-
;;; term/eshell/packages.el

(package! eshell-up :pin "9c100bae5c3020e8d9307e4332d3b64e7dc28519")
(package! eshell-z :pin "337cb241e17bd472bd3677ff166a0800f684213c")
(package! shrink-path :pin "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
(package! esh-help :pin "417673ed18a983930a66a6692dbfb288a995cb80")

(when (featurep! :completion company)
  (package! fish-completion :pin "10384881817b5ae38cf6197a077a663420090d2c")
  (package! bash-completion :pin "96ce14af9674f3e605bacca87abc0c23b8f13cd5"))
