;; -*- no-byte-compile: t; -*-
;;; feature/syntax-checker/packages.el

(package! flycheck)
(package! flycheck-popup-tip)
(when EMACS26+
  (package! flycheck-posframe))
