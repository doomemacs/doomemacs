;;; lang/python/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "python")
  (warn! "Python isn't installed."))

(when (executable-find "pyenv")
  (unless (split-string (shell-command-to-string "pyenv versions --bare") "\n" t)
    (warn! "No versions of python are available via pyenv, did you forget to install one?")))
