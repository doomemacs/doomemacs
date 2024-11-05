;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "e171dacc12eeaba5dbd7fd887f94d11b357fe958")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
(package! yaml-mode :pin "d91f878729312a6beed77e6637c60497c5786efa")

(when (modulep! :completion company)
  (package! company-ansible :pin "338922601cf9e8ada863fe6f2dd9d5145d9983b0"))
