;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/clojure/doctor.el

(when (and (modulep! :checkers syntax)
           (not (modulep! +lsp)))
  (unless (executable-find "clj-kondo")
    (warn! "Couldn't find clj-kondo. flycheck-clj-kondo will not work.")))
