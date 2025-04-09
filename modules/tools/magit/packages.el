;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "bf58615a033b8c827bf630962531c67539789215") ; 4.3.2
(when (modulep! +forge)
  (package! forge :pin "9db4d386a1ce32b574e413771996d41d9b2407e8") ; 0.5.0
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
