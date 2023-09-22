;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! ruby-mode :built-in t)
(package! yard-mode :pin "de1701753a64544c3376b015805f3661136d8038")

;; REPL
(package! inf-ruby :pin "03475ac1cca410ee63a523f5c63f859cfafe1aeb")
(when (modulep! :completion company)
  (package! company-inf-ruby :pin "fe3e4863bc971fbb81edad447efad5795ead1b17"))

;; Programming environment
(package! rubocop :pin "f5fd18aa810c3d3269188cbbd731ddc09006f8f5")
(package! robe :pin "912ae2ba1f467bd55b2da64bfec9db3f8a723916")

;; Project tools
(package! bundler :pin "43efb6be4ed118b06d787ce7fbcffd68a31732a7")
(package! rake :pin "452ea0caca33376487103c64177c295ed2960cca")

;; Environment management
(when (modulep! +rbenv)
  (package! rbenv :pin "2ea1a5bdc1266caef1dd77700f2c8f42429b03f1"))
(when (modulep! +rvm)
  (package! rvm :pin "e1e83b5466c132c066142ac63729ba833c530c83"))
(when (modulep! +chruby)
  (package! chruby :pin "42bc6d521f832eca8e2ba210f30d03ad5529788f"))

;; Testing frameworks
(package! rspec-mode :pin "29df3d081c6a1cbdf840cd13d45ea1c100c5bbaa")
(package! minitest :pin "ddd152c990a528ad09a696bfad23afa4330ea4d7")

;; Rails
(when (modulep! +rails)
  (package! projectile-rails :pin "701784df7befe17b861f1b53fe9cbc59d0b94b9f")
  (package! inflections :pin "55caa66a7cc6e0b1a76143fd40eff38416928941"))
