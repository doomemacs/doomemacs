;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "cf6b8f06c2628357fc2a72ea9817a2c2d0ebf690")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "ecd19a40b7832bb00f0a2244e3b0713d0bf3850d")
(package! yaml-mode :pin "68fecb5f0dec712a10c8655df6881392a4613617")

(when (featurep! :completion company)
  (package! company-ansible :pin "79dd421b161efa49fbdffad57fa40edb41f484a3"))
