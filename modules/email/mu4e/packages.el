;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (featurep! +org)
  (package! org-msg :pin "4c92c627b6cfb234fd257b714a5dbfc72d7af8d2"))

(package! mu4e-alert :pin "91f0657c5b245a9de57aa38391221fb5d141d9bd")

(package! mu4e-thread-folding :recipe (:host github :repo "rougier/mu4e-thread-folding")
  :pin "56bb25a1addba4c6ed79c4e5d1a580d80cc698f2")
