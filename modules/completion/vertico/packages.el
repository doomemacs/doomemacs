;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "cd3c400aac2f5435080dc55d13c03c8886241365")

(package! orderless :pin "62f71c34baca0b7d0adeab4a1c07d85ffcee80d9")

(package! consult :pin "166e3b03c3de4f88bbfdeef7f52efac27642e2d3")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "8a0b80c6e6ecdf6bb0df7dc496fe4d03197def65")
(package! embark-consult :pin "8a0b80c6e6ecdf6bb0df7dc496fe4d03197def65")

(package! marginalia :pin "fbd2f378f532b6d34d95d84b43edabd00e99a472")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "a0f34d68cc12330ab3992a7521f9caa1de3b8470"))
