;;; lang/purescript/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find "purs-tidy")
    (warn! "Could not find purs-tidy. Formatting will be disabled.")))
