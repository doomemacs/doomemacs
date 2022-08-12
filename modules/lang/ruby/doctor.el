;;; lang/ruby/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "ruby")
  (warn! "Ruby isn't installed."))

(when (and (executable-find "rbenv") (modulep! +rbenv))
  (unless (split-string (shell-command-to-string "rbenv versions --bare") "\n" t)
    (warn! "No versions of ruby are available via rbenv, did you forget to install one?")))

(when (and (executable-find "chruby") (modulep! +chruby))
  (unless (split-string (shell-command-to-string "chruby") "\n" t)
    (warn! "No versions of ruby are available via chruby, did you forget to install one?")))
