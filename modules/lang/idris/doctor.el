;;; lang/idris/doctor.el -*- lexical-binding: t; -*-

(when (require 'idris-mode nil t)
  (unless (executable-find idris-interpreter-path)
    (warn! "Cannot find the idris interpreter. Most features will not work.")))
