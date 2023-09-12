;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/ido/doctor.el

(dolist (module '(helm ivy vertico))
  (when (doom-module-p :completion module)
    (error! "This module is incompatible with :completion %s; disable one or the other"
            module)))
