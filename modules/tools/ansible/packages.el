;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "eebb2fb49d3c0a0586d1e4ead9ba618c7d003cae")
(package! ansible-doc :pin "86083a7bb2ed0468ca64e52076b06441a2f8e9e0")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
(package! yaml-mode :pin "7b5ce294fb15c2c8926fa476d7218aa415550a2a")

(when (modulep! :completion company)
  (package! company-ansible :pin "338922601cf9e8ada863fe6f2dd9d5145d9983b0"))
