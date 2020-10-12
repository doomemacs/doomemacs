;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(package! geiser :pin "2accab72e289ed82707237d2013ba034c88ff6c2")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "2940f1622fa352e7ca95a9e4ad65958c5575da02"))
