;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck)
(package! flycheck-popup-tip)
(when (featurep! +childframe)
  (package! flycheck-posframe))

;; TODO flymake?
