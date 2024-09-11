;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! ruby-mode :built-in t)
(package! yard-mode :pin "de1701753a64544c3376b015805f3661136d8038")

;; REPL
(package! inf-ruby :pin "b234625c85a48cc71e7045f5d48f079f410d576a")
(when (modulep! :completion company)
  (package! company-inf-ruby :pin "fe3e4863bc971fbb81edad447efad5795ead1b17"))

;; Programming environment
(package! rubocop :pin "f5fd18aa810c3d3269188cbbd731ddc09006f8f5")
(package! robe :pin "6bc8a07fc483407971de0966d367a11006b3ab80")

;; Project tools
(package! bundler :pin "43efb6be4ed118b06d787ce7fbcffd68a31732a7")
(package! rake :pin "452ea0caca33376487103c64177c295ed2960cca")

;; Environment management
(when (modulep! +rbenv)
  (package! rbenv :pin "588b817d510737b9d6afd6d1ecddd517d96b78e5"))
(when (modulep! +rvm)
  (package! rvm :pin "e1e83b5466c132c066142ac63729ba833c530c83"))
(when (modulep! +chruby)
  (package! chruby :pin "42bc6d521f832eca8e2ba210f30d03ad5529788f"))

;; Testing frameworks
(package! rspec-mode :pin "29df3d081c6a1cbdf840cd13d45ea1c100c5bbaa")
(package! minitest :pin "5999c45c047212cee15a2be67e78787776a79c35")

;; Refactoring
(package! ruby-json-to-hash :pin "383b22bb2e007289ac0dba146787d02ff99d4415")

;; Rails
(when (modulep! +rails)
  (package! rails-routes :pin "eab995a9297ca5bd9bd4f4c2737f2fecfc36def0")
  (package! rails-i18n :pin "8e87e4e48e31902b8259ded28a208c2e7efea6e9")
  (package! projectile-rails :pin "701784df7befe17b861f1b53fe9cbc59d0b94b9f")
  (package! inflections :pin "55caa66a7cc6e0b1a76143fd40eff38416928941"))
