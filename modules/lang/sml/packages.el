;; -*- no-byte-compile: t; -*-
;;; lang/sml/packages.el

(package! sml-mode :pin "d114e5a27f3d213b3e32c02f9320cd2041b6376b")
(when (modulep! :completion company)
  (package! company-mlton
    :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis"))
    :pin "9b09d209b4767a2af24784fb5321390ed1d445bf"))
