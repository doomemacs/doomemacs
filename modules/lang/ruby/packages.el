;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

(package! inf-ruby)
(package! rspec-mode)
(package! ruby-refactor)
(package! yard-mode)

(when (featurep! :completion company)
  (package! company-inf-ruby))

;;
(def-bootstrap! ruby
  ;; Since there are so many possible setups for a ruby environment (rbenv, rvm,
  ;; etc), I'll leave it to you and only take care of installing gem
  ;; dependencies.
  (unless (executable-find "ruby")
    (error "ruby isn't installed"))
  (unless (executable-find "gem")
    (error "gem isn't installed"))
  (when-let (pkgs (cl-remove-if 'executable-find) '("ruby-lint")))
    (sh "gem install %s" (s-join " " pkgs)))
