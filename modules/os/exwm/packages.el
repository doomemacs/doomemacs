;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! exwm :pin "10bd12234e896d35a2c4eafabc62a31126d23bf3")
(when (featurep! :editor evil)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil")
    :pin "373c2f52f46b742622a92352d7a035ccde1aee00")
  (package! exwm-firefox-evil :pin "14643ee53a506ddcb5d2e06cb9f1be7310cd00b1"))
(package! exwm-edit :pin "2fd9426922c8394ec8d21c50dcc20b7d03af21e4")
(package! language-detection :pin "54a6ecf55304fba7d215ef38a4ec96daff2f35a4")
(package! exwm-mff :pin "89206f2e3189f589c27c56bd2b6203e906ee7100")
