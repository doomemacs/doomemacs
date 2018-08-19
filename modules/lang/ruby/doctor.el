;;; lang/ruby/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "ruby")
  (warn! "Ruby isn't installed."))

(when (executable-find "rbenv")
  (unless (split-string (shell-command-to-string "rbenv versions --bare") "\n" t)
    (warn! "No versions of ruby are available via rbenv, did you forget to install one?")))
