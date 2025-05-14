;; -*- no-byte-compile: t; -*-
;;; lang/sml/packages.el

(package! sml-mode :pin "c33659fd9b62fab436366f731daa4339691dd6bf")
(when (modulep! :completion company)
  (package! company-mlton
    :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis"))
    :pin "9b09d209b4767a2af24784fb5321390ed1d445bf"))
