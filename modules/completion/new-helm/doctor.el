;;; completion/new-helm/doctor.el -*- lexical-binding: t; -*-

(when (featurep! +ack-grep)
  (unless (executable-find "ack")
    (warn! "Couldn't find ack executable; helm-do-grep-1 will not work")))
