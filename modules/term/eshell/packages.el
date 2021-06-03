;; -*- no-byte-compile: t; -*-
;;; term/eshell/packages.el

(package! eshell-up :pin "ff84e6069b98f2ed00857a0f78bff19d96e4955c")
(package! eshell-z :pin "337cb241e17bd472bd3677ff166a0800f684213c")
(package! shrink-path :pin "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
(package! esh-help :pin "417673ed18a983930a66a6692dbfb288a995cb80")
(package! eshell-did-you-mean :pin "7cb6ef8e2274d0a50a9e114d412307a6543533d5")
(package! eshell-syntax-highlighting :pin "32d2568ebeb42553a30dda77e03c0e2ec8854199")

(unless IS-WINDOWS
  (when (featurep! :completion company)
    (package! fish-completion :pin "10384881817b5ae38cf6197a077a663420090d2c")
    (package! bash-completion :pin "65e54c6f9c0ffebf94f7c505694bd249b9b53d32")))
