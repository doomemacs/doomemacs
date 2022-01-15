;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "75f33e36204f8a72d0b8cde81e04d3350a848981")

(package! orderless :pin "62f71c34baca0b7d0adeab4a1c07d85ffcee80d9")

(package! consult :pin "57dc1adfdc0feafc71c6f418ff7aa1adbe47a5fd")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "5b34b2b60370cea5ad9da5931856667a6fae0501")
(package! embark-consult :pin "5b34b2b60370cea5ad9da5931856667a6fae0501")

(package! marginalia :pin "678b6528f3905e624b01daf787461d8c7e06ec0f")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "a0f34d68cc12330ab3992a7521f9caa1de3b8470"))
