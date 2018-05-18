;; -*- no-byte-compile: t; -*-
;;; lang/solidity/packages.el

(package! solidity-mode)
(when (featurep! :completion company)
  (package! company-solidity))
