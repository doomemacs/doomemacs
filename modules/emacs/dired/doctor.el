;;; emacs/dired/doctor.el -*- lexical-binding: t; -*-

(when (and (featurep :system 'bsd) (not (executable-find "gls")))
  (warn! "Cannot find gls (GNU ls). This may cause issues with dired"))
