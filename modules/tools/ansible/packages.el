;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "c6532e52161a381ed3dddfeaa7c92ae636d3f052")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "4540f99a3e363403a633587e05a9707605c16473")
(package! yaml-mode :pin "34648f2502f52f4744d62758fa381fa35db1da49")

(when (featurep! :completion company)
  (package! company-ansible :pin "79dd421b161efa49fbdffad57fa40edb41f484a3"))
