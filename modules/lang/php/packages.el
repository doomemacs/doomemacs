;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "21250984ad8137aa3440ac12e52475ef03f19fcb")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "535aec81739e8e766e0420fda616efc8846f2911")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "fe6bc91c3bd8b329c6d26ad883a025f06b5121ee")
(package! composer :pin "7c7f89df226cac69664d7eca5e913b544dc475c5")

(when (featurep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "4c1c2b093970b92f8589b061759288c0deb228c9"))

(unless (featurep! +lsp)
  (package! phpactor :pin "272217fbb6b7e7f70615fc518d77c6d75f33a44f")
  (when (featurep! :completion company)
    (package! company-phpactor :pin "272217fbb6b7e7f70615fc518d77c6d75f33a44f")))

(when (featurep! :editor format)
  (package! php-cs-fixer :pin "7e12a1af5d65cd8a801eeb5564c6268a4e190c0c"))

;; For building php-extras
(package! async :pin "5d365ffc6a2c2041657eaa5d762c395ea748c8d7")
