;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "ba650a7ab90d66686ba787937ac9e71f749c598e")

(package! orderless :pin "53f5204ad3f541e11eb6eeb9b86584964b7a3678")

(package! consult :pin "fe4852280006e61be7f1374d021ee06155ce5a26")
(package! consult-dir :pin "15891383f34d43acc5bb82bda92239b1f54cf178")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "754f5497d827f7d58009256a21af614cc44378a3"))
(package! embark :pin "9c166c4b96a0b1e85401bcc6fb95ce021e7b5013")
(package! embark-consult :pin "9c166c4b96a0b1e85401bcc6fb95ce021e7b5013")

(package! marginalia :pin "da72da4622c7b38741e6968678028f7e0564816c")

(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "2e0e09e5bbd6ec576ddbe566ab122575ef051fab"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "834d39acfe8a7d2c304afbe4d649b9372118c756"))
