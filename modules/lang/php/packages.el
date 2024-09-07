;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "ae15a36301a49e5ae03118ff815a6a511603ae13")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "59814bd80c59894022bc5950fb3bdf02420e8a89")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
(package! composer :pin "791a7104be2ef2748757a186094c1e8f7f531a01")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "343e45f2a616c726f20ba099f3f98a1a01cec405"))

;; For building php-extras
(package! async :pin "43f97d7e689eea65043ded1464747a2f3ce7661c")
