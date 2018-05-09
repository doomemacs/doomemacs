;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; feature/snippets/doctor.el

(require 'yasnippet)
(unless (ignore-errors (yas-reload-all)
                       (yas--get-snippet-tables))
  (warn! "Couldn't find any snippets in any of these directories: %s" yas-snippet-dirs))
