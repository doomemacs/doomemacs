;; -*- no-byte-compile: t; -*-
;;; term/eshell/packages.el

(package! eshell-up :pin "ff84e6069b98f2ed00857a0f78bff19d96e4955c")
(package! eshell-z :pin "337cb241e17bd472bd3677ff166a0800f684213c")
(package! shrink-path :pin "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
(package! esh-help :pin "417673ed18a983930a66a6692dbfb288a995cb80")
(package! eshell-did-you-mean :pin "80cd8c4b186a2fb29621cf634bcf2bcd914f1e3d")
(package! eshell-syntax-highlighting :pin "8e3a685fc6d97af79e1046e5b24385786d8e92f6")

(unless IS-WINDOWS
  (package! fish-completion :pin "10384881817b5ae38cf6197a077a663420090d2c")
  (package! bash-completion :pin "c5eaeed156ab906190c662d491269230967104b1"))
