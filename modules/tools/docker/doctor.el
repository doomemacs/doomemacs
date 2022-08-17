;;; tools/docker/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find "dockfmt")
    (warn! "Couldn't find dockfmt. Formatting will be disabled.")))
