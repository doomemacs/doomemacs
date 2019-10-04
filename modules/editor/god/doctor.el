;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; editor/god/doctor.el

(when (featurep! :editor evil)
  (warn! "god-mode is not really compatible with evil"))

(when (featurep! :editor objed)
  (warn! "god-mode is not really compatible with objed"))
