;; -*- no-byte-compile: t; -*-
;;; lang/solidity/packages.el

(package! solidity-mode :pin "6f7bd1641e5282ec5163188d8b8c2f6dfddc2e36")
(when (featurep! :completion company)
  (package! company-solidity))
(when (featurep! :checkers syntax)
  (package! solidity-flycheck))
