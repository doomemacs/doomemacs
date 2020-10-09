;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (featurep! :lang org)
  (package! org-msg :pin "2db6725c4a4f4342a9c61895b7c3c82795b01fee"))

(package! mu4e-alert :pin "91f0657c5b245a9de57aa38391221fb5d141d9bd")
