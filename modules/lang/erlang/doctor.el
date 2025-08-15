;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/erlang/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(when (modulep! :editor format)
  (unless (and (executable-find "rebar3") (zerop (car (doom-call-process "rebar3" "fmt" "-v"))))
    (warn! "Couldn't find erlfmt. Formatting will be disabled.")))
