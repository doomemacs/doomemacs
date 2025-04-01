;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "28d272ce0bcecc2e312d22ed15a48ad4cea564eb") ; 4.3.1
(when (modulep! +forge)
  (package! forge :pin "1c904090dfdcd201d9170997052c43846ddce149") ; 0.4.8
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
