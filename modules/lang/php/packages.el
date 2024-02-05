;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "c7dde979d9fd3ce59771f050bd38b2796cace446")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "a0bcafbe30494b2c5a70c2fe05f2bb0859e83645")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
(package! composer :pin "91945f1bdb655be272320d14dab306b661a128a1")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "278e4cc4032bff92060496cf1179643cfc6f9c0f"))

(unless (modulep! +lsp)
  (package! phpactor :pin "8733fef84b458457c1bfd188cfb861fc3150ee1c")
  (when (modulep! :completion company)
    (package! company-phpactor :pin "8733fef84b458457c1bfd188cfb861fc3150ee1c")))

(when (modulep! :editor format)
  (package! php-cs-fixer :pin "efe4368d891f1eec6311363cfd6be3e9eadb5e0a"))

;; For building php-extras
(package! async :pin "d040f72cb0be5265d50ac541ddb09ebbc68b7908")
