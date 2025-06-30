;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/clojure/doctor.el

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(when (and (modulep! :checkers syntax)
           (modulep! -lsp))
  (unless (executable-find "clj-kondo")
    (warn! "Couldn't find clj-kondo. flycheck-clj-kondo will not work.")))

(when (modulep! :editor format)
  (unless (executable-find "cljfmt")
    (warn! "Couldn't find cljfmt. Formatting will be disabled.")))
