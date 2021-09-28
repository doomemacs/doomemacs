;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (featurep! +org)
  (package! org-msg :pin "599e8b056c30e84d584aa54dd7c85339cdb9dc43"))

(package! mu4e-alert :pin "91f0657c5b245a9de57aa38391221fb5d141d9bd")
