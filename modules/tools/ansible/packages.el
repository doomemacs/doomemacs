;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "d89ac0ee57742cca0f0e0a3453d9dcc521575690")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
(package! yaml-mode :pin "5b58248ab255dff6cfa4c4057a191bc4446ee5b6")

(when (modulep! :completion company)
  (package! company-ansible :pin "79dd421b161efa49fbdffad57fa40edb41f484a3"))
