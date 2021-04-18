;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc :pin "cbbd2ea1e94a36004432a9ac61414cb5a95a39bd")
(package! go-guru :pin "49a538028e63dbe20f428c52d91f09b70b564626")
(package! go-mode :pin "49a538028e63dbe20f428c52d91f09b70b564626")
(package! gorepl-mode :pin "6a73bf352e8d893f89cad36c958c4db2b5e35e07")
(package! go-tag :pin "59b243f2fa079d9de9d56f6e2d94397e9560310a")
(package! go-gen-test :pin "44c202ac97e728e93a35cee028a0ea8dd6e4292c")

(when (featurep! :completion company)
  (package! company-go :pin "4acdcbdea79de6b3dee1c637eca5cbea0fdbe37c"))

(when (featurep! :checkers syntax)
  (package! flycheck-golangci-lint :pin "8e446c68311048f0b87febf8ef0379e29d358851"))
