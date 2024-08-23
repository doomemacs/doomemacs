;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "ae15a36301a49e5ae03118ff815a6a511603ae13")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "4792988a120d6ac515ba16605278d04cb8be0d69")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
(package! composer :pin "42cf9848d438f8dc4c07ac684a83280ace7bb94c")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "ccf20511f0f2ed45d00d423c703bb91ab6a8b80c"))

;; For building php-extras
(package! async :pin "cff2bd0be3c78a2eb76717eed60302972fe9b8c5")
