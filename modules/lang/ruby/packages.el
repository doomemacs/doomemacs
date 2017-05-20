;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; requires ruby ruby-lint

(package! inf-ruby)
(package! rspec-mode)
(package! ruby-refactor)
(package! yard-mode)

(when (featurep! :completion company)
  (package! company-inf-ruby))

