;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "ae15a36301a49e5ae03118ff815a6a511603ae13")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "31f702ee2de35d514fb633c0c37531cb648bff70")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
(package! composer :pin "6c7e19256ff964546cea682edd21446c465a663c")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "343e45f2a616c726f20ba099f3f98a1a01cec405"))

;; For building php-extras
(package! async :pin "af47d6f930f93d4fdc4ca46e19e17547bf486c4c")
