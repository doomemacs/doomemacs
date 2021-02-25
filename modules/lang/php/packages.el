;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris :pin "f2faebf610c917f7091f7ec0cd97645629c4f819")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "8cdc727e6d4eef81655b90574784e9540f407cda")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "fe6bc91c3bd8b329c6d26ad883a025f06b5121ee")

(when (featurep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "847fd910e9d0ac76e2cfeb87512e6923a39d7d5f"))

(unless (featurep! +lsp)
  (package! phpactor :pin "62d2372ea55c0c5fb4e77076988472ebb5d85f24")
  (when (featurep! :completion company)
    (package! company-phpactor :pin "62d2372ea55c0c5fb4e77076988472ebb5d85f24")))

(when (featurep! :editor format)
  (package! php-cs-fixer :pin "c5b5d8a4986b31bade5e2a57131469bf90630db8"))

;; For building php-extras
(package! async :pin "d7e7f79ee42311a0187aa2ab4f4e2f8843fa28da")
