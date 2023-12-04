;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico  :pin "e8edac107df5883f2fa8690356a8d8deee1a83de")

(package! orderless :pin "b24748093b00b37c3a572c4909f61c08fa27504f")

(package! consult :pin "9c7dbbe4d626987e40d7a157430021e13a6906bd")
(package! consult-dir :pin "3f5f4b71ebe819392cb090cda71bd39a93bd830a")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "0662839aa5db429130f5ffd15c14d4a980b2e694"))
(package! embark :pin "b9f2b3b9a5b9c72cf1416097b9941c4f275dae94")
(package! embark-consult :pin "b9f2b3b9a5b9c72cf1416097b9941c4f275dae94")

(package! marginalia :pin "27ccfd2213bb9432883427cf058c63af10196aa6")

(package! wgrep :pin "3132abd3750b8c87cbcf6942db952acfab5edccd")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "15168c92ca06a39fbf84ac27d4e2ae02216300b5"))
