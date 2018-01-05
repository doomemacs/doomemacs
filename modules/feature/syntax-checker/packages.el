;; -*- no-byte-compile: t; -*-
;;; feature/syntax-checker/packages.el

(package! flycheck)
(if IS-MAC
    (package! flycheck-popup-tip)
  (package! flycheck-pos-tip))
