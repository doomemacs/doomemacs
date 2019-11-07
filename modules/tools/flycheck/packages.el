;; -*- no-byte-compile: t; -*-
;;; tools/flycheck/packages.el

(package! flycheck)
(package! flycheck-popup-tip)
(when (featurep! +childframe)
  (package! flycheck-posframe))
