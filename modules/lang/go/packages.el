;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc :pin "cbbd2ea1e94a36004432a9ac61414cb5a95a39bd")
(package! go-guru :pin "166dfb1e090233c4609a50c2ec9f57f113c1da72")
(package! go-mode :pin "166dfb1e090233c4609a50c2ec9f57f113c1da72")
(package! gorepl-mode :pin "6a73bf352e8d893f89cad36c958c4db2b5e35e07")
(package! go-tag :pin "33f2059551d5298ca228d90f525b99d1a8d70364")
(package! go-gen-test :pin "f84f4177af7fcbe10ce2116d5417ad5f0485034b")

(when (modulep! :completion company)
  (package! company-go :pin "31948b463f2fc18f8801e5a8fe511fef300eb3dd"))

(when (modulep! :checkers syntax)
  (package! flycheck-golangci-lint :pin "8e446c68311048f0b87febf8ef0379e29d358851"))
