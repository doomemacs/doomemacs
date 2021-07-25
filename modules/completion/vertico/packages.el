;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "4a9029714e847832d3ecb3ae74a7049306924f2e")

(package! orderless :pin "1e84120a28525ccb47b602fc19b7afbeffbbe502")

(package! consult :pin "28f9ba8bdfdb13257862a658715b6ceb96f4951e")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "be03ce9ce1630b32e29cc50118d058c05696cb35")
(package! embark-consult :pin "be03ce9ce1630b32e29cc50118d058c05696cb35")

(package! marginalia :pin "a3a8edbf25db4b1e167f1fdff6f60a065d0bf9cb")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion
    :recipe (:host github :repo "iyefrat/all-the-icons-completion")
    :pin "24cdb3b42c6ca0a8926ad6958c76d7928fc559ce"))
