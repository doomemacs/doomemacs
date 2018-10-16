;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/ocaml/doctor.el

(unless (executable-find "ocamlmerlin")
  (warn! "Couldn't find ocamlmerlin. Lookup, completion and syntax checking won't work"))

;; Tuareg can still indent
(unless (executable-find "ocp-indent")
  (warn! "Couldn't find ocp-indent. Auto-indentation will be less precise"))

(when (featurep! :feature eval)
  (unless (executable-find "utop")
    (warn! "Couldn't find utop. REPL won't be available")))

(unless (executable-find "dune")
  (warn! "Couldn't find dune. Won't be able to highlight dune files"))

(when (featurep! :editor format)
  (unless (executable-find "ocamlformat")
    (warn! "Couldn't find ocamlformat. Code-formatting will be unavailable")))
