;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! ruby-mode :built-in t)
(package! yard-mode :pin "ba74a47463b0320ae152bd42a7dd7aeecd7b5748")

;; REPL
(package! inf-ruby :pin "1fc972ecda6a2155806d75d202481327703a0681")
(when (featurep! :completion company)
  (package! company-inf-ruby :pin "fe3e4863bc971fbb81edad447efad5795ead1b17"))

;; Programming environment
(package! rubocop :pin "1372ee3fc1daf7dc8d96741b03e4aff5f7ae3906")
(package! robe :pin "3ef165c5c99aebfd811a0f18ea7f8c983d4ab152")

;; Project tools
(package! bundler :pin "43efb6be4ed118b06d787ce7fbcffd68a31732a7")
(package! rake :pin "9c204334b03b4e899fadae6e59c20cf105404128")

;; Environment management
(when (featurep! +rbenv)
  (package! rbenv :pin "2ea1a5bdc1266caef1dd77700f2c8f42429b03f1"))
(when (featurep! +rvm)
  (package! rvm :pin "c1f2642434b0f68d9baa0687127079ecd884ba12"))
(when (featurep! +chruby)
  (package! chruby :pin "42bc6d521f832eca8e2ba210f30d03ad5529788f"))

;; Testing frameworks
(package! rspec-mode :pin "92ef785010f6a68cbf73861e75ac6cf4e1832291")
(package! minitest :pin "ddd152c990a528ad09a696bfad23afa4330ea4d7")

;; Rails
(when (featurep! +rails)
  (package! projectile-rails :pin "8d6b3734958f5dc7a620dc1b44754986d3726f3d")
  (package! inflections :pin "55caa66a7cc6e0b1a76143fd40eff38416928941"))
