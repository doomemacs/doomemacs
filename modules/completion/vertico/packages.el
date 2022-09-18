;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "2ad46196653b8a873adf11aee949d621af8ff6bc")

(package! orderless :pin "8b9af2796fa0eb87eea4140bc08d16880a493803")

(package! consult :pin "6319aec3513cd587a8817269bc32c8283d419710")
(package! compat :pin "cc1924fd8b3f9b75b26bf93f084ea938c06f9615")
(package! consult-dir :pin "d397ca6ea67af4d3c59a330a778affd825f0efd9")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "9b40f136c017fadf6239d7602d16bf73b4ad5198"))

(package! embark :pin "5d0459d27aa7cf738b5af36cf862723a62bef955")
(package! embark-consult :pin "5d0459d27aa7cf738b5af36cf862723a62bef955")

(package! marginalia :pin "69442c2d9472b665f698f67426cd255f6c0620a3")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "286e2c064a1298be0d8d4100dc91d7a7a554d04a"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "7ca364d319e7ba8ccba26a0d57513f3e66f1b05b"))
