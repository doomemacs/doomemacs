;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "eedcb847869226701acaf9a36dce0a51d1b60862")

(package! orderless :pin "1ccf74ffdbb0dd34caa63022e92f947c09c49c86")

(package! consult :pin "cc8eff9578f5d089735e8b7dd7a08732890ed648")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "0ad7e8ff15683a4d64b79c29b3fcf847edfe244b"))

(package! embark :pin "e08899ef2e7fb9c1ed4b4b21e44cd368561f91f9")
(package! embark-consult :pin "e08899ef2e7fb9c1ed4b4b21e44cd368561f91f9")

(package! marginalia :pin "2fb2787bc302a5533e09bc558c76eb914e98543b")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "a0f34d68cc12330ab3992a7521f9caa1de3b8470"))
