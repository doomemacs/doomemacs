;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico  :pin "03bfb71a2b5db296a93eeb0752bd934d112e0792")

(package! orderless :pin "d6b402a89e234d0e6166247ed6025f9acc8b4d9a")

(package! consult :pin "43380042daaaf57f60aa22962a97ed904a7d56ce")
(package! consult-dir :pin "3f5f4b71ebe819392cb090cda71bd39a93bd830a")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "3f2a7c17cc2fe64e0c07e3bf90e33c885c0d7062"))
(package! embark :pin "7758a1ac64d784fa71f14b202b7a26c27f29d03e")
(package! embark-consult :pin "7758a1ac64d784fa71f14b202b7a26c27f29d03e")

(package! marginalia :pin "f1734375a5d8fa18e9cecb47ae4b5ae86c72399f")

(package! wgrep :pin "3132abd3750b8c87cbcf6942db952acfab5edccd")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "db9fbc95bb8316165ec74e500a76d6857e6ced1a"))
