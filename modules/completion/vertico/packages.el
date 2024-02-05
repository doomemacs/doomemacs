;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico  :pin "4a7da56b02c6aefff8f6b4574a530a7cb54bc21a")

(package! orderless :pin "b24748093b00b37c3a572c4909f61c08fa27504f")

(package! consult :pin "9463146ba754103db9475ae56e46561366ba4773")
(package! consult-dir :pin "3f5f4b71ebe819392cb090cda71bd39a93bd830a")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "d83f87581af74f7a2739d8b1b90c37da5ae3d310"))
(package! embark :pin "60139db8794f7e4a08076d9f7597d08f6c8083d1")
(package! embark-consult :pin "60139db8794f7e4a08076d9f7597d08f6c8083d1")

(package! marginalia :pin "ea356ebb1ddb8d6da78574b517155475cf52d46f")

(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "2e0e09e5bbd6ec576ddbe566ab122575ef051fab"))
