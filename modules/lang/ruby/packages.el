;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! enh-ruby-mode :pin "732331b99a")
(package! yard-mode :pin "ba74a47463")

;; REPL
(package! inf-ruby :pin "fd8d392fef")
(when (featurep! :completion company)
  (package! company-inf-ruby :pin "fe3e4863bc"))

;; Programming environment
(package! rubocop :pin "03bf15558a")
(package! robe :pin "8190cb7c7b")

;; Project tools
(package! bundler :pin "43efb6be4e")
(package! rake :pin "9c204334b0")

;; Environment management
(when (featurep! +rbenv)
  (package! rbenv :pin "2ea1a5bdc1"))
(when (featurep! +rvm)
  (package! rvm :pin "134497bc46"))

;; Testing frameworks
(package! rspec-mode :pin "c4353a1bff")
(package! minitest :pin "6d9f6233b7")
