;; -*- no-byte-compile: t; -*-
;;; lang/solidity/packages.el

(package! solidity-mode :pin "ddb4a901c33bd29ba2eefff3b7f7461849c6e833")
(when (modulep! :completion company)
  (package! company-solidity))
(when (modulep! :checkers syntax)
  (package! solidity-flycheck))
