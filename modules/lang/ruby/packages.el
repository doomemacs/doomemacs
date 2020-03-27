;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! ruby-mode :built-in t)
(package! yard-mode :pin "ba74a47463")

;; REPL
(package! inf-ruby :pin "41e5ed3a88")
(when (featurep! :completion company)
  (package! company-inf-ruby :pin "fe3e4863bc"))

;; Programming environment
(package! rubocop :pin "03bf15558a")
(package! robe :pin "68503b32bb")

;; Project tools
(package! bundler :pin "43efb6be4e")
(package! rake :pin "9c204334b0")

;; Environment management
(when (featurep! +rbenv)
  (package! rbenv :pin "2ea1a5bdc1"))
(when (featurep! +rvm)
  (package! rvm :pin "134497bc46"))
(when (featurep! +chruby)
  (package! chruby :pin "42bc6d521f"))

;; Testing frameworks
(package! rspec-mode :pin "9a2a9d2935")
(package! minitest :pin "97d7d1760b")

;; Rails
(when (featurep! +rails)
  (package! projectile-rails :pin "0398d940a2")
  (package! inflections :pin "e4f1372cf2"))
