;;; tools/collab/doctor.el -*- lexical-binding: t; -*-

(when (and (modulep! +tunnel)
           (not (executable-find "tuntox")))
  (warn! "Couldn't find tuntox command. This needs to be on your path for the +tunnel flag to work properly."))
