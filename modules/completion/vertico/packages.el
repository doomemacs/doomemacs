;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "7ec0f0c0769496edf4c8376950c2fdeb44b98102")

(package! orderless :pin "8f64537f556f26492fe5ee401d8d578d7d88684b")

(package! consult :pin "36b8bc7065005778db83f12a594b6929bfd3319c")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "9b40f136c017fadf6239d7602d16bf73b4ad5198"))

(package! embark :pin "2890e535f55b1f08f379fd761b263fa337a72185")
(package! embark-consult :pin "2890e535f55b1f08f379fd761b263fa337a72185")

(package! marginalia :pin "a514c024ac2796ec9d52f65c1f51b51f96bcb1c7")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "9e7d456b0934ecb568b6f05a8445e3f4ce32261f"))
