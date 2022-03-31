;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh :pin "21250984ad8137aa3440ac12e52475ef03f19fcb")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "5f26bec865ee159dc30d3922f17bc42adfcfed50")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "fe6bc91c3bd8b329c6d26ad883a025f06b5121ee")
(package! composer :pin "7c7f89df226cac69664d7eca5e913b544dc475c5")

(when (featurep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "a522f61c088ee2a13ab17f289a3131329e59badf"))

(unless (featurep! +lsp)
  (package! phpactor :pin "34195f1533209e2ffd0f898a69c7db2bffd1eabe")
  (when (featurep! :completion company)
    (package! company-phpactor :pin "34195f1533209e2ffd0f898a69c7db2bffd1eabe")))

(when (featurep! :editor format)
  (package! php-cs-fixer :pin "7e12a1af5d65cd8a801eeb5564c6268a4e190c0c"))

;; For building php-extras
(package! async :pin "c78bab7506a70a735d2c3deab13fa87bf44a83d3")
