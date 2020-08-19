;; -*- no-byte-compile: t; -*-
;;; lang/sml/packages.el

(package! sml-mode :pin "60b01d7ebc600b61603d20a049570843b5e047d3")
(when (featurep! :completion company)
  (package! company-mlton
    :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis"))
    :pin "b87e36348fff9fa060b780c2019330ac5d5665ec"))
