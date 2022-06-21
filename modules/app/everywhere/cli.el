;;; app/everywhere/cli.el -*- lexical-binding: t; -*-

(defcli! () ()
  "Spawn an emacsclient window for quick edits."
  (exit! "emacsclient --eval '(emacs-everywhere)'"))
