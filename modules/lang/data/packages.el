;; -*- no-byte-compile: t; -*-
;;; lang/data/packages.el

(package! graphql-mode)
(package! json-mode)
(package! jsonnet-mode)
(package! yaml-mode)
(package! csv-mode)
(package! dhall-mode)
(package! protobuf-mode :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*")))

;; DEPRECATED `conf-toml-mode' exists in Emacs 26+
(unless (fboundp 'conf-toml-mode)
  (package! toml-mode))
