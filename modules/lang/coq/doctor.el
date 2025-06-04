;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/coq/doctor.el

(unless (modulep! +no-opam)
  (unless (executable-find "opam")
    (warn! "Couldn't find OPAM")))
