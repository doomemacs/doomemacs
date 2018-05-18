;; -*- no-byte-compile: t; -*-
;;; feature/syntax-checker/packages.el

(package! flycheck)
(package! flycheck-popup-tip)
(when (and EMACS26+ (featurep! +childframe))
  (package! flycheck-posframe))
