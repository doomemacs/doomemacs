;;; ui/posframe/doctor.el -*- lexical-binding: t; -*-

(when (version< emacs-version "26")
  (error! "This module doesn't work in Emacs %s (minimum: Emacs 26)" emacs-version))
