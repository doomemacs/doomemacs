;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; editor/meow/doctor.el

(when (featurep! :editor evil)
  (warn! "meow is not compatible with evil"))

(when (featurep! :editor objed)
  (warn! "meow is not compatible with objed"))

(when (featurep! :editor god)
  (warn! "meow is not compatible with god-mode"))
