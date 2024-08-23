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
    :pin "ccf20511f0f2ed45d00d423c703bb91ab6a8b80c"))

;; For building php-extras
(package! async :pin "f317b0c9c3e60a959f45d035ed5e31a78f1263ac")
