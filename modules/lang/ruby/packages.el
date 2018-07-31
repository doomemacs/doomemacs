;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; requires ruby ruby-lint

(package! enh-ruby-mode)
(package! rubocop)
(package! inf-ruby)
(package! rspec-mode)
(package! yard-mode)
(package! rake)
(package! robe)

(when (featurep! :completion company)
  (package! company-inf-ruby))

(when (featurep! +rbenv)
  (package! rbenv))
(when (featurep! +rvm)
  (package! rvm))
