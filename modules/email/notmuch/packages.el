;; -*- no-byte-compile: t; -*-
;;; email/notmuch/packages.el

(package! notmuch)
(package! org-mime)
(when (featurep! :completion ivy)
  (package! counsel-notmuch))
(when (featurep! :completion helm)
  (package! helm-notmuch))
