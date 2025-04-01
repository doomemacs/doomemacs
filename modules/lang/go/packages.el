;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc :pin "cbbd2ea1e94a36004432a9ac61414cb5a95a39bd")
(package! go-mode :pin "0ed3c5227e7f622589f1411b4939c3ee34711ebd")
(package! gorepl-mode :pin "6a73bf352e8d893f89cad36c958c4db2b5e35e07")
(package! go-tag :pin "33f2059551d5298ca228d90f525b99d1a8d70364")
(package! go-gen-test :pin "af00a9abbaba2068502327ecdef574fd894a884b")

(when (modulep! :completion company)
  (package! company-go :pin "31948b463f2fc18f8801e5a8fe511fef300eb3dd"))

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-golangci-lint :pin "424ba1b3a13f5548c440b7a25822932ad4b51cd6"))
