;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! enh-ruby-mode)
(package! yard-mode)

;; REPL
(package! inf-ruby)
(when (featurep! :completion company)
  (package! company-inf-ruby))

;; Programming environment
(package! rubocop)
(package! robe)

;; Project tools
(package! bundler)
(package! rake)

;; Environment management
(when (featurep! +rbenv)
  (package! rbenv))
(when (featurep! +rvm)
  (package! rvm))

;; Testing frameworks
(package! rspec-mode)
(package! minitest)
