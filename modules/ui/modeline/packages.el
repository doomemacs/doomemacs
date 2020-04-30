;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "c177959bbfa7fa6f199b1145c6986e55f462f1c1"))
(package! anzu :pin "3e34fb3df53c0c68e842fa179c327a7395d1901d")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "9bca6ca14d865e7e005bc02a28a09b4ae74facc9"))
