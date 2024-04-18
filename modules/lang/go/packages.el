;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc :pin "cbbd2ea1e94a36004432a9ac61414cb5a95a39bd")
(package! go-guru :pin "6f4ff9ef874d151ed8d297a80f1bf27db5d9dbf0")
(package! go-mode :pin "6f4ff9ef874d151ed8d297a80f1bf27db5d9dbf0")
(package! gorepl-mode :pin "6a73bf352e8d893f89cad36c958c4db2b5e35e07")
(package! go-tag :pin "33f2059551d5298ca228d90f525b99d1a8d70364")
(package! go-gen-test :pin "af00a9abbaba2068502327ecdef574fd894a884b")

(when (modulep! :completion company)
  (package! company-go :pin "31948b463f2fc18f8801e5a8fe511fef300eb3dd"))

(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-golangci-lint :pin "91c59b128aa6f719069cfb3e5df77588691a3e14"))
