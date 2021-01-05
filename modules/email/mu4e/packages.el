;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (featurep! +org)
  (package! org-msg :pin "bb378c7942804b81ac9ddf4b14381cd9d84c993c"))

(package! mu4e-alert :pin "91f0657c5b245a9de57aa38391221fb5d141d9bd")
