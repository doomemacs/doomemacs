;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris :pin "f2faebf610")
(package! php-extras :recipe (:host github :repo "arnested/php-extras") :pin "d410c5af66")
(package! php-mode :pin "cade4cef2b")
(package! php-refactor-mode :pin "7a794b0618")
(package! phpunit :pin "fe6bc91c3b")

(when (featurep! +hack)
  (package! hack-mode :recipe (:host github :repo "hhvm/hack-mode") :pin "fd6a661b09"))

(unless (featurep! +lsp)
  (package! phpactor :pin "5ccf65d59e")
  (when (featurep! :completion company)
    (package! company-phpactor :pin "5ccf65d59e")))

(when (featurep! :editor format)
  (package! php-cs-fixer :pin "6540006710"))

;; For building php-extras
(package! async :pin "86aef2c38e")
