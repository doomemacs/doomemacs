;; -*- no-byte-compile: t; -*-
;;; lang/solidity/packages.el

(package! solidity-mode :pin "20fb77e089e10187b37ae1a94153017b82ed2a0a")
(when (modulep! :completion company)
  (package! company-solidity))
(when (modulep! :checkers syntax)
  (package! solidity-flycheck))
