;;; app/everywhere/cli.el -*- lexical-binding: t; -*-

(defcli! everywhere ()
  "Spawn an emacsclient window for quick edits."
  (throw 'exit (list "emacsclient" "--eval" "(emacs-everywhere)")))
