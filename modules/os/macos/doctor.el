;;; os/macos/doctor.el -*- lexical-binding: t; -*-

;; See https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-08/msg00560.html
(when (version< emacs-version "29.3")
  (warn! "There are bugs in multi-user Keychain integration on Emacs <=29.2")
  (explain! "See https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-08/msg00560.html"))
