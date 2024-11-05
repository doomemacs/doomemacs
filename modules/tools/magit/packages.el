;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "f2a61334430291d2162a68138c95ab310a8557f1") ; 4.1.0
(when (modulep! +forge)
  (package! forge :pin "9f2efc3c03719af60be6f9da2835336aedb522be") ; 0.4.3
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
