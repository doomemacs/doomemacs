;;; completion/ivy/doctor.el -*- lexical-binding: t; -*-

(when (and (not EMACS26+) (featurep! +childframe))
  (error! "The +childframe feature requires Emacs 26+"))
