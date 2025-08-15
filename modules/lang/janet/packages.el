;; -*- no-byte-compile: t; -*-
;;; lang/janet/packages.el

(package! janet-mode
  :recipe (:files ("*.el"))
  :pin "9e3254a0249d720d5fa5603f1f8c3ed0612695af")

;; (when (modulep! +tree-sitter)
;;   (package! janet-ts-mode
;;     :recipe (:host github :repo "sogaiu/janet-ts-mode")
;;     :pin "ac684edf57e4d4e085cf99d5ad2ee084b46b8123"))
