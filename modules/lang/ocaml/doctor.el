;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/ocaml/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "ocamlmerlin")
  (warn! "Couldn't find ocamlmerlin. Lookup, completion and syntax checking won't work"))

;; Tuareg can still indent
(unless (executable-find "ocp-indent")
  (warn! "Couldn't find ocp-indent. Auto-indentation will be less precise"))

(when (modulep! :tools eval)
  (unless (executable-find "utop")
    (warn! "Couldn't find utop. REPL won't be available")))

(unless (executable-find "dune")
  (warn! "Couldn't find dune. Won't be able to highlight dune files"))

(when (modulep! :editor format)
  (unless (executable-find "ocamlformat")
    (warn! "Couldn't find ocamlformat. Code-formatting will be unavailable")))
