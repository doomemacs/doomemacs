;;; lang/json/doctor.el -*- lexical-binding: t; -*-

(when (and (featurep! :completion ivy)
           (not (executable-find "jq")))
  (warn! "Couldn't find jq. counsel-jq won't work." ))
