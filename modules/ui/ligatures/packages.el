;; -*- no-byte-compile: t; -*-
;;; ui/ligeratures/packages.el

(when (and (or (featurep 'ns)
               (featurep 'harfbuzz))
           (featurep 'composite))
  (package! ligature :pin "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
