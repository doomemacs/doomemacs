;; -*- no-byte-compile: t; -*-
;;; lang/solidity/packages.el

(package! solidity-mode :pin "8ba549e429e86778a0e079648f3bc3463fcb15f6")
(when (modulep! :completion company)
  (package! company-solidity))
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! solidity-flycheck))
