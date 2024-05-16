;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "68e51fda552a2f91caab69e83564bc91275b09b1")

(package! orderless :pin "ac4aeb66f331f4c4a430d5556071e33177304c37")

(package! consult :pin "6eba1a3fa8e13681091a30b2490a03bdce5f243a")
(package! consult-dir :pin "3f5f4b71ebe819392cb090cda71bd39a93bd830a")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "754f5497d827f7d58009256a21af614cc44378a3"))
(package! embark :pin "d3c9d1b4c890cf365846cc2b418f37341999e79f")
(package! embark-consult :pin "d3c9d1b4c890cf365846cc2b418f37341999e79f")

(package! marginalia :pin "3275d1f85cb020280979a050054b843f7563aea2")

(package! wgrep :pin "208b9d01cfffa71037527e3a324684b3ce45ddc4")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "2e0e09e5bbd6ec576ddbe566ab122575ef051fab"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "834d39acfe8a7d2c304afbe4d649b9372118c756"))
