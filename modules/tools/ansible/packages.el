;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! ansible :recipe (:nonrecursive t) :pin "c6532e5216")
(package! ansible-doc :pin "86083a7bb2")
(package! jinja2-mode :pin "cfaa7bbe7b")
(package! yaml-mode :pin "cecf4b106b")

(when (featurep! :completion company)
  (package! company-ansible :pin "79dd421b16"))
