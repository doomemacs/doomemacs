;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris :pin "f2faebf610c917f7091f7ec0cd97645629c4f819")
(package! php-extras :recipe (:host github :repo "arnested/php-extras") :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "167b35749dbf700543d4a540d098c015af58df2b")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "fe6bc91c3bd8b329c6d26ad883a025f06b5121ee")

(when (featurep! +hack)
  (package! hack-mode :recipe (:host github :repo "hhvm/hack-mode") :pin "fd6a661b091490920804d043303596f9e60a5dd7"))

(unless (featurep! +lsp)
  (package! phpactor :pin "19d56b4c62772f6939cf1576c72213bf72fd3eb1")
  (when (featurep! :completion company)
    (package! company-phpactor :pin "19d56b4c62772f6939cf1576c72213bf72fd3eb1")))

(when (featurep! :editor format)
  (package! php-cs-fixer :pin "6540006710daf2b2d47576968ea826a83a40a6bf"))

;; For building php-extras
(package! async :pin "86aef2c38e7d35e8509b7feeee3e989d825eba91")
