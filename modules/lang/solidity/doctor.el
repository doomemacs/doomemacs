;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/solidity/doctor.el

(unless (executable-find "solc")
  (warn! "Solc isn't installed."))

(unless (executable-find "solium -V")
  (warn! "Solium isn't installed."))
