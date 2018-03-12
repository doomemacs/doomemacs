;; -*- no-byte-compile: t; -*-
;;; ui/posframe/packages.el

(when (featurep! :completion company)
  (package! company-childframe))
(when (featurep! :completion ivy)
  (package! ivy-posframe))
