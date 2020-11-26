;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "bbf1e7339eba06784dfe86643bb0fbddf5bb0342")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "e897897d6a92b4ffdbff33acf69f5c661da1123a")

(unless (featurep! :editor evil)
  (package! expand-region :pin "ea6b4cbb9985ddae532bd2faf9bb00570c9f2781"))
