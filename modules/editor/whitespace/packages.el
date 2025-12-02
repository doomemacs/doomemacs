;; -*- no-byte-compile: t; -*-
;;; editor/whitespace/packages.el

(when (modulep! +guess)
  (package! dtrt-indent :pin "7c372bec8d84c247e4bd0d5599024d66ee300429"))

(when (modulep! +trim)
  (package! ws-butler
    ;; REVIEW: emacsmirror/nongnu_elpa serves this package from a branch. To stop
    ;;   Straight from clobbering a single repo for multiple packages, we must be
    ;;   explicit to force it to clone it multiple times.
    :recipe (:host github
             :repo "emacsmirror/nongnu_elpa"
             :branch "elpa/ws-butler"
             :local-repo "ws-butler")
    :pin "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
