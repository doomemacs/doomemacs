;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "e1faeb01ed379dd773116adbc22e8ca52f6f8162")

(package! orderless :pin "2646dad28c0819fbe9ee521d39efb9ae40e03982")

(package! consult :pin "5fb6248c8e12630ce1247985c67ea28ae4077e4f")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "9d56be162badbbfee405595f2ebdfe16a5bca47d")
(package! embark-consult :pin "9d56be162badbbfee405595f2ebdfe16a5bca47d")

(package! marginalia :pin "d4c2028c7917b2ff926b3a67c3acc0351be658cc")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion
    :recipe (:host github :repo "iyefrat/all-the-icons-completion")
    :pin "24cdb3b42c6ca0a8926ad6958c76d7928fc559ce"))
