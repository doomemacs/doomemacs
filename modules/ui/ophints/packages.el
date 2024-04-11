;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(cond ((modulep! :editor evil) (package! evil-goggles :pin "34ca276a85f615d2b45e714c9f8b5875bcb676f3"))
      ((modulep! +goggles) (package! goggles :pin "41d3669d7ae7b73bd39d298e5373ece48b656ce3"))
      (t (package! volatile-highlights :pin "fcf6e2778454ce514c189a7d1fe70e03ad81c325")))
