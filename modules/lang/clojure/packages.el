;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! cider :pin "ba5680b066121a06bbf9442e54d555de38bdefb2")
(package! clj-refactor :pin "e24ba6284317dbb3e678fcad325044c628da56da")

(when (featurep! :checkers syntax)
  (package! flycheck-joker :pin "51e99e697761ee8dab863930910abdba7607c1bd"))
