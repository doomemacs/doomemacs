;; -*- no-byte-compile: t; -*-
;;; lang/data/packages.el

(package! graphql-mode :pin "7c37aee28b")
(package! json-mode :pin "0e819e519a")
(package! jsonnet-mode :pin "2b90b4e12a")
(package! yaml-mode :pin "cecf4b106b")
(package! csv-mode :pin "fbf942e127")
(package! dhall-mode :pin "ef4d33debe")
(package! protobuf-mode
  :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*"))
  :pin "94b7bd7e8b")
