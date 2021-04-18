;;; emacs/dired/doctor.el -*- lexical-binding: t; -*-

(when (and IS-BSD (not (executable-find "gls")))
  (warn! "Cannot find gls (GNU ls). This may cause issues with dired"))
