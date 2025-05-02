;; -*- no-byte-compile: t; -*-
;;; term/eshell/packages.el

(package! eshell-up :pin "1999afaa509204b780db44e99ac9648fe7d92d32")
(package! eshell-z :pin "337cb241e17bd472bd3677ff166a0800f684213c")
(package! shrink-path :pin "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
(package! esh-help :pin "417673ed18a983930a66a6692dbfb288a995cb80")
(package! eshell-did-you-mean :pin "80cd8c4b186a2fb29621cf634bcf2bcd914f1e3d")
(package! eshell-syntax-highlighting :pin "62418fd8b2380114a3f6dad699c1ba45329db1d2")

(unless (featurep :system 'windows)
  (package! fish-completion :pin "1256f137a2039805d4e87f8e6c11a162ed019587")
  (package! bash-completion :pin "d0637428fd0592ef56baa0255673300129f98c48"))
