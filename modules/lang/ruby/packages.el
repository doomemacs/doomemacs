;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; requires ruby ruby-lint

(package! enh-ruby-mode)
(package! yard-mode)
(package! inf-ruby)
(package! robe)

(when (featurep! :completion company)
  (package! company-inf-ruby))

;; Project tools
(package! bundler)
(package! rake)
(package! rubocop)

;; Version management
(when (featurep! +rbenv)
  (package! rbenv))
(when (featurep! +rvm)
  (package! rvm))

;; Testing frameworks
(package! rspec-mode)
(package! minitest)
