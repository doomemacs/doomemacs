;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "223bf55975d148f65f66bb8980c14c1230f067ff")
(package! php-mode :pin "d9858333e42f42c1486a84bc5277e9d8e37e40cc")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
(package! composer :pin "eba6b953a4a68bc22b957d59eb3159c6d5cff39d")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "0addbff8b61cfd75b81961507a1646c4acd316ba"))
