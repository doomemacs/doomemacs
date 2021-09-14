;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris :pin "f2faebf610c917f7091f7ec0cd97645629c4f819")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "535aec81739e8e766e0420fda616efc8846f2911")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "fe6bc91c3bd8b329c6d26ad883a025f06b5121ee")

(when (featurep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "4c1c2b093970b92f8589b061759288c0deb228c9"))

(unless (featurep! +lsp)
  (package! phpactor :pin "272217fbb6b7e7f70615fc518d77c6d75f33a44f")
  (when (featurep! :completion company)
    (package! company-phpactor :pin "272217fbb6b7e7f70615fc518d77c6d75f33a44f")))

(when (featurep! :editor format)
  (package! php-cs-fixer :pin "cc9a3624dcdc72d748d91e3d7cdb8544a1d85a51"))

;; For building php-extras
(package! async :pin "5d365ffc6a2c2041657eaa5d762c395ea748c8d7")
