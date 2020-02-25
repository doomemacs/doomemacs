;;; lang/ruby/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "ruby")
  (warn! "Ruby isn't installed."))

(when (and (executable-find "rbenv") (featurep! +rbenv))
  (unless (split-string (shell-command-to-string "rbenv versions --bare") "\n" t)
    (warn! "No versions of ruby are available via rbenv, did you forget to install one?")))
