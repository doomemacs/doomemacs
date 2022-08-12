;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; editor/god/doctor.el

(when (modulep! :editor evil)
  (warn! "god-mode is not really compatible with evil"))

(when (modulep! :editor objed)
  (warn! "god-mode is not really compatible with objed"))
