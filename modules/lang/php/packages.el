;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris)
(package! php-extras :recipe (:fetcher github :repo "arnested/php-extras"))
(package! php-mode)
(package! php-refactor-mode)
(package! phpunit)

(when (featurep! :completion company)
  (package! company-php))

;; (package! hack-mode
;;   :recipe
;;   (:fetcher url :url "https://raw.githubusercontent.com/facebook/hhvm/master/hphp/hack/editor-plugins/emacs/hack-mode.el"))

