;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode)
(when (featurep! +flutter)
  (package! flutter))
