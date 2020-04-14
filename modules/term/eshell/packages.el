;; -*- no-byte-compile: t; -*-
;;; term/eshell/packages.el

(package! eshell-up :pin "9c100bae5c")
(package! eshell-z :pin "337cb241e1")
(package! shrink-path :pin "c14882c859")
(package! esh-help :pin "417673ed18")

(when (featurep! :completion company)
  (package! fish-completion :pin "1038488181")
  (package! bash-completion :pin "96ce14af96"))
