;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/ivy/doctor.el

(when (and (not EMACS26+) (featurep! +childframe))
  (error! "The +childframe feature requires Emacs 26+"))
