;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "68cbd47589446e9674921bae0b98ff8fbe28be23")

(package! orderless :pin "dc7a781acf2e58ac7d20d1b522be0cde5213e057")

(package! consult :pin "b48ff6bf0527baeb6bfd07c6da9d303ff0b79c3d")
(package! consult-dir :pin "3f5f4b71ebe819392cb090cda71bd39a93bd830a")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "754f5497d827f7d58009256a21af614cc44378a3"))
(package! embark :pin "c93abadc8220c0caa6fea805f7a736c346d47e7e")
(package! embark-consult :pin "c93abadc8220c0caa6fea805f7a736c346d47e7e")

(package! marginalia :pin "f6fe86b989a177355ab3ff7e97a384e10a7b0bb1")

(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "2e0e09e5bbd6ec576ddbe566ab122575ef051fab"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "834d39acfe8a7d2c304afbe4d649b9372118c756"))
