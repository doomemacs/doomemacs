;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "cc5f5421c6270b8fdd12265ccdaffc3cd297d0d8")

(package! orderless :pin "75eeae21971d86b51a712ed8ecd6434463b2d866")

(package! consult :pin "822928a8609730e8c22e068b04d7908312706cfd")
(package! consult-dir :pin "d397ca6ea67af4d3c59a330a778affd825f0efd9")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "9b40f136c017fadf6239d7602d16bf73b4ad5198"))

(package! embark :pin "d88478b45f2d589339334dc8d40b07bce28aab0e")
(package! embark-consult :pin "d88478b45f2d589339334dc8d40b07bce28aab0e")

(package! marginalia :pin "26f2bd9ee7b63bcad6604108e2f565b34bc6083b")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "286e2c064a1298be0d8d4100dc91d7a7a554d04a"))

(when (featurep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "7ca364d319e7ba8ccba26a0d57513f3e66f1b05b"))
