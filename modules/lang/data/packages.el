;; -*- no-byte-compile: t; -*-
;;; lang/data/packages.el

(package! graphql-mode :pin "7c37aee28bf8c8ffb3da73df5571c4e1e352562b")
(package! json-mode :pin "0e819e519ae17a2686e0881c4ca51fa873fa9b83")
(package! jsonnet-mode :pin "2b90b4e12a11c42df0f1e5db327a50555b6ff023")
(package! yaml-mode :pin "cecf4b106b0c4236931b14919fdf87ff3546e2c9")
(package! csv-mode :pin "fbf942e127e68ac8cfcd08a53500ca554fcac079")
(package! dhall-mode :pin "ef4d33debe224c6ba37e51a29b9dc8b74f20f1c2")
(package! protobuf-mode
  :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*"))
  :pin "94b7bd7e8b87ff100c603153d2f8d7a2a08ab50b")
