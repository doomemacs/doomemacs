;; -*- no-byte-compile: t; -*-
;;; app/write/packages.el

(package! synosaurus)
(package! mixed-pitch)

(when (featurep! +langtool)
  (package! langtool))
(when (featurep! +wordnut)
  (package! wordnut))

