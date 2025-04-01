;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "ae15a36301a49e5ae03118ff815a6a511603ae13")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "5b6cc1c068ba759dcf30067ad1e048b3693a40d7")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
(package! composer :pin "6c7e19256ff964546cea682edd21446c465a663c")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "0addbff8b61cfd75b81961507a1646c4acd316ba"))

;; For building php-extras
(package! async :pin "bb3f31966ed65a76abe6fa4f80a960a2917f554e")
