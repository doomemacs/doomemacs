;; -*- no-byte-compile: t; -*-
;;; ui/posframe/packages.el

(when EMACS26+
  (when (featurep! :completion ivy)
    (package! ivy-posframe)))
