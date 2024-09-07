;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "0aa26864e3fc4e6949711a4821caf6819e7ab171") ; 4.1.0
(when (modulep! +forge)
  (package! forge :pin "d4e88507bf0d256fd92c8d5ccdbee8f7ccbb99b3") ; 0.4.3
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
