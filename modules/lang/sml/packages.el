;; -*- no-byte-compile: t; -*-
;;; lang/sml/packages.el

(package! sml-mode :pin "5426ff47382441cf079a75ab7fa0a1c90730bf09")
(when (modulep! :completion company)
  (package! company-mlton
    :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis"))
    :pin "9b09d209b4767a2af24784fb5321390ed1d445bf"))
