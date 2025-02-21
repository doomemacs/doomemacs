;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "f52dfada8fa0fa6cd70886819868d84e198773a3") ; 4.3.0
(when (modulep! +forge)
  (package! forge :pin "33e240d360b8e3950e9c8b7024e3e763465c0c13") ; 0.4.7
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
