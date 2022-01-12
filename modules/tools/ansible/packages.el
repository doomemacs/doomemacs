;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "40af0d2bbb6c5bbcf7aa9269ac9a07e22622d263")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "ecd19a40b7832bb00f0a2244e3b0713d0bf3850d")
(package! yaml-mode :pin "a79d2a7b9281f8c56f461d717b1ba40fc58e22fd")

(when (featurep! :completion company)
  (package! company-ansible :pin "79dd421b161efa49fbdffad57fa40edb41f484a3"))
