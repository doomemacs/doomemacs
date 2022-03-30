;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "a92b1e47ffe343e2c3096e2ea61af013a8a02af9")

(package! orderless :pin "f2c78c4a6059c5f892e48a3887d4368a547515ff")

(package! consult :pin "473e6585c516d0e7fd4c256c333713fb40e9947a")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "0ad7e8ff15683a4d64b79c29b3fcf847edfe244b"))

(package! embark :pin "06d5caafd58db6b6d7fa14cf8b6f7336486b92ca")
(package! embark-consult :pin "06d5caafd58db6b6d7fa14cf8b6f7336486b92ca")

(package! marginalia :pin "e63d27e6fb24ed16339de9d813c555d40aa1e4ca")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "9e7d456b0934ecb568b6f05a8445e3f4ce32261f"))
