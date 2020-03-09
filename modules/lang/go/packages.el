;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc :pin "cbbd2ea1e9")
(package! go-guru :pin "85a20dac6c")
(package! go-mode :pin "85a20dac6c")
(package! gorepl-mode :pin "6a73bf352e")
(package! go-tag :pin "59b243f2fa")
(package! go-gen-test :pin "44c202ac97")

(when (featurep! :completion company)
  (package! company-go :pin "939b4a677f"))

(when (featurep! :checkers syntax)
  (package! flycheck-golangci-lint :pin "8e446c6831"))
