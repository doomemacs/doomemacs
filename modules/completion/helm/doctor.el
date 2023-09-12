;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/helm/doctor.el

(dolist (module '(ivy ido vertico))
  (when (doom-module-p :completion module)
    (error! "This module is incompatible with :completion %s; disable one or the other"
            module)))
