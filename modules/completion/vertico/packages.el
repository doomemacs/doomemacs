;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "46e8e0565079b7161ada4beb94c8938ee9c04bfb")

(package! orderless :pin "8f64537f556f26492fe5ee401d8d578d7d88684b")

(package! consult :pin "d30213aa209391e03b1c1011df92d91a1fc5ef32")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "9b40f136c017fadf6239d7602d16bf73b4ad5198"))

(package! embark :pin "2890e535f55b1f08f379fd761b263fa337a72185")
(package! embark-consult :pin "2890e535f55b1f08f379fd761b263fa337a72185")

(package! marginalia :pin "dbc37b373e734269bd75d1763e7309863508bf10")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "286e2c064a1298be0d8d4100dc91d7a7a554d04a"))

(when (featurep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "7ca364d319e7ba8ccba26a0d57513f3e66f1b05b"))
