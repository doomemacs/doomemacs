;;; lang/json/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "jq")
  (warn! "Couldn't find jq. counsel-jq won't work."))
