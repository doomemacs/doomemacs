;; -*- no-byte-compile: t; -*-
;;; ui/posframe/packages.el

(when EMACS26+
  (when (featurep! :completion company)
    (package! company-box))
  (when (featurep! :completion ivy)
    (package! ivy-posframe)))
