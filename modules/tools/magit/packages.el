;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "8b6bb7c7e88c298e1447ca7c86138588a3953784") ; 4.1.0
(when (modulep! +forge)
  (package! forge :pin "0c9060626200902f7b0078a85566ef0eea8cc0b6") ; 0.4.6
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
