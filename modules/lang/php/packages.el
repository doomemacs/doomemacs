;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris :pin "f2faebf610c917f7091f7ec0cd97645629c4f819")
(package! php-extras
  :recipe (:host github :repo "arnested/php-extras")
  :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
(package! php-mode :pin "cbf27232649c39e3749eefd23f962750bd249a49")
(package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
(package! phpunit :pin "fe6bc91c3bd8b329c6d26ad883a025f06b5121ee")

(when (featurep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")
    :pin "f9315be69954b95b6a3ceaa37f31a88f8369a59f"))

(unless (featurep! +lsp)
  (package! phpactor :pin "272217fbb6b7e7f70615fc518d77c6d75f33a44f")
  (when (featurep! :completion company)
    (package! company-phpactor :pin "272217fbb6b7e7f70615fc518d77c6d75f33a44f")))

(when (featurep! :editor format)
  (package! php-cs-fixer :pin "c5b5d8a4986b31bade5e2a57131469bf90630db8"))

;; For building php-extras
(package! async :pin "9a8cd0c3d5c120bfa03187c54dba6e33f6e3ca19")
