;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "ae15a36301a49e5ae03118ff815a6a511603ae13")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "0f756a8c0782ebdc00557addc68763305c91ca51")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
(package! composer :pin "6c7e19256ff964546cea682edd21446c465a663c")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "343e45f2a616c726f20ba099f3f98a1a01cec405"))

;; For building php-extras
(package! async :pin "b99658e831bc7e7d20ed4bb0a85bdb5c7dd74142")
