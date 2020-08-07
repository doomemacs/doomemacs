;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "b5ef59406604bc5027f4d816d90e633feef0149c")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "ecd19a40b7832bb00f0a2244e3b0713d0bf3850d")
(package! yaml-mode :pin "34648f2502f52f4744d62758fa381fa35db1da49")

(when (featurep! :completion company)
  (package! company-ansible :pin "79dd421b161efa49fbdffad57fa40edb41f484a3"))
