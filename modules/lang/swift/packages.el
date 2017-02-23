;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode)

(when (featurep! :completion company)
  (package! company-sourcekit))
