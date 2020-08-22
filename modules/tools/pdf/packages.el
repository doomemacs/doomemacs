;; -*- no-byte-compile: t; -*-
;;; tools/pdf/packages.el

(package! pdf-tools
  ;; REVIEW waiting on politza/pdf-tools#588
  :recipe (:host github
           :repo "flatwhatson/pdf-tools"
           :branch "fix-macros")
  :pin "86db180be75254072dee5ea1611a4e8b9c0648de")
