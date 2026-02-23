;;; lang/hledger/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "hledger")
  (warn! "hledger isn't installed"))
