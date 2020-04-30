;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "c6532e52161a381ed3dddfeaa7c92ae636d3f052")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "cfaa7bbe7bb290cc500440124ce89686f3e26f86")
(package! yaml-mode :pin "cecf4b106b0c4236931b14919fdf87ff3546e2c9")

(when (featurep! :completion company)
  (package! company-ansible :pin "79dd421b161efa49fbdffad57fa40edb41f484a3"))
