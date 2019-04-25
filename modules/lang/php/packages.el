;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris)
(package! php-extras :recipe (:fetcher github :repo "arnested/php-extras"))
(package! php-mode)
(package! php-refactor-mode)
(package! phpunit)

(when (featurep! +hack)
  (package! hack-mode :recipe (:fetcher github :repo "hhvm/hack-mode")))

(unless (featurep! +lsp)
  (package! phpactor))

(when (featurep! :editor format)
  (package! php-cs-fixer))
