;; -*- no-byte-compile: t; -*-
;;; ui/ligatures/packages.el

(when (and (or (featurep 'ns)
               (string-match-p "HARFBUZZ" system-configuration-features))
           (featurep 'composite))
  (package! ligature :pin "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
