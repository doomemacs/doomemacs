;;; lang/ledger/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "ledger")
  (warn! "ledger isn't installed"))
