;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! cider)
(package! clj-refactor)

(when (and (featurep! :feature syntax-checker)
           (featurep! +joker))
  (package! flycheck-joker))
