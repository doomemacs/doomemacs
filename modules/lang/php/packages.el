;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "796b26a5cd75df9d2ecb206718b310ff21787063")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "fb11df8268b7099766264cd53836ef159746adbd")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "4a36906344c0abc11f48cc08cd8d50a9f46963f8")
(package! composer :pin "5af1707fefbd9d7db1102afdaeb8f2da893fea37")

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "26f06ffe82574f98e7da381e48202eceb8ef0793"))

(unless (modulep! +lsp)
  (package! phpactor :pin "9440005814c4858880ad45afdaa7d2e637a9d280")
  (when (modulep! :completion company)
    (package! company-phpactor :pin "9440005814c4858880ad45afdaa7d2e637a9d280")))

(when (modulep! :editor format)
  (package! php-cs-fixer :pin "efe4368d891f1eec6311363cfd6be3e9eadb5e0a"))

;; For building php-extras
(package! async :pin "71cc50f27ffc598a89aeaa593488d87818647d02")
