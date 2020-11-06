;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! ruby-mode :built-in t)
(package! yard-mode :pin "ba74a47463b0320ae152bd42a7dd7aeecd7b5748")

;; REPL
(package! inf-ruby :pin "b0124578db09b64902c8ca981b02e74bec92ef36")
(when (featurep! :completion company)
  (package! company-inf-ruby :pin "fe3e4863bc971fbb81edad447efad5795ead1b17"))

;; Programming environment
(package! rubocop :pin "03bf15558a6eb65e4f74000cab29412efd46660e")
(package! robe :pin "082da38797d247f9c64568ad712a345e041d5c84")

;; Project tools
(package! bundler :pin "43efb6be4ed118b06d787ce7fbcffd68a31732a7")
(package! rake :pin "9c204334b03b4e899fadae6e59c20cf105404128")

;; Environment management
(when (featurep! +rbenv)
  (package! rbenv :pin "2ea1a5bdc1266caef1dd77700f2c8f42429b03f1"))
(when (featurep! +rvm)
  (package! rvm :pin "6897576bb068b967347fabd7fb15f4ae7ec13462"))
(when (featurep! +chruby)
  (package! chruby :pin "42bc6d521f832eca8e2ba210f30d03ad5529788f"))

;; Testing frameworks
(package! rspec-mode :pin "f1029cad2a8d9fd096e9a0f1ae9e7d7e2c8bd1ec")
(package! minitest :pin "ddd152c990a528ad09a696bfad23afa4330ea4d7")

;; Rails
(when (featurep! +rails)
  (package! projectile-rails :pin "7a256b1b1444fe0001f97095d99252e946dd9777")
  (package! inflections :pin "e4f1372cf22e811faca52fc86bdd5d817498a4d8"))
