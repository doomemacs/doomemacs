;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "8b6bb7c7e88c298e1447ca7c86138588a3953784") ; 4.1.0
(when (modulep! +forge)
  (package! forge :pin "96fe98120c492f698088126b5340edc1976e7508") ; 0.4.3
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
