;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! enh-ruby-mode :pin "732331b99a0884dd7fc0149658d4090886857656")
(package! yard-mode :pin "ba74a47463b0320ae152bd42a7dd7aeecd7b5748")

;; REPL
(package! inf-ruby :pin "fd8d392fefd1d99eb58fc597d537d0d7df29c334")
(when (featurep! :completion company)
  (package! company-inf-ruby :pin "fe3e4863bc971fbb81edad447efad5795ead1b17"))

;; Programming environment
(package! rubocop :pin "03bf15558a6eb65e4f74000cab29412efd46660e")
(package! robe :pin "8190cb7c7beb8385dd3abf6ea357f33d8981ae8a")

;; Project tools
(package! bundler :pin "05a91d68e21e129b6c4d5462c888ea249c2ea001")
(package! rake :pin "9c204334b03b4e899fadae6e59c20cf105404128")

;; Environment management
(when (featurep! +rbenv)
  (package! rbenv :pin "2ea1a5bdc1266caef1dd77700f2c8f42429b03f1"))
(when (featurep! +rvm)
  (package! rvm :pin "134497bc460990c71ab8fa75431156e62c17da2d"))

;; Testing frameworks
(package! rspec-mode :pin "c4353a1bff164bccf6c55fda16aa7b9c9ab36685")
(package! minitest :pin "6d9f6233b7ce63c63c96675514c228fd93a2b6a1")
