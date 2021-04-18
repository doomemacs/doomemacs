;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/solidity/doctor.el

(when (require 'solidity-common nil t)
  (unless (executable-find solidity-solc-path)
    (warn! "Solc isn't installed."))
  (unless (executable-find solidity-solium-path)
    (warn! "Solium isn't installed.")))
