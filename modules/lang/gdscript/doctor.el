;;; lang/gdscript/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find "gdformat")
    (warn! "Couldn't find gdformat. Formatting will be disabled.")))
