;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/erlang/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(when (modulep! :editor format)
  (unless (and (executable-find "rebar3")
               (zerop (car (doom-call-process "rebar3" "format"))))
    (warn! "Couldn't find rebar3 with format plugin installed. Formatting will be disabled.")))
