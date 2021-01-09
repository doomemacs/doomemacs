;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/ansible/doctor.el

(unless (executable-find "ansible")
  (warn! "Couldn't find ansible executable. Some features of the ansible module won't work"))
