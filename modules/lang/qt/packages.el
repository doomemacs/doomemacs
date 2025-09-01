;; -*- no-byte-compile: t; -*-
;;; lang/qt/packages.el

(package! qml-mode :pin "6c5f33ba88ae010bf201a80ee8095e20a724558c")
(package! qt-pro-mode :pin "7a2da323de834294b413cbbb3c92f42f54913643")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! qml-ts-mode
    :recipe (:host github
             :repo "xhcoding/qml-ts-mode")
    :pin "b80c6663521b4d0083e416e6712ebc02d37b7aec"))
