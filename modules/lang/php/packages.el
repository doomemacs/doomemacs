;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris)
(package! php-extras :recipe (:host github :repo "arnested/php-extras"))
(package! php-mode)
(package! php-refactor-mode)
(package! phpunit)

(when (featurep! +hack)
  (package! hack-mode :recipe (:host github :repo "hhvm/hack-mode")))

(unless (featurep! +lsp)
  (package! phpactor)
  (when (featurep! :completion company)
    (package! company-phpactor)))

(when (featurep! :editor format)
  (package! php-cs-fixer))
