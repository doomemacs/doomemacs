;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico  :pin "cf8b2abf5207696c054c33214c86e3969d415054")

(package! orderless :pin "b24748093b00b37c3a572c4909f61c08fa27504f")

(package! consult :pin "e4d371235647a7f4967f093eff2125652796957c")
(package! consult-dir :pin "3f5f4b71ebe819392cb090cda71bd39a93bd830a")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "d83f87581af74f7a2739d8b1b90c37da5ae3d310"))
(package! embark :pin "33c392cf3ce5b92ad73ed5c4f44dbca5d0741cde")
(package! embark-consult :pin "33c392cf3ce5b92ad73ed5c4f44dbca5d0741cde")

(package! marginalia :pin "ea356ebb1ddb8d6da78574b517155475cf52d46f")

(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "bc0e67cbbba4daaf6ce7b8701a0dc7797d468752"))
