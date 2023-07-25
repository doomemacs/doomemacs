;; -*- no-byte-compile: t; -*-
;;; lang/raku/packages.el

(package! raku-mode :pin "977b14a7c1295ebf2aad2f807d3f8e7c27aeb47f")

(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "b1acccd6e9d9753022571ee96d18b8e9f3227c65"))
