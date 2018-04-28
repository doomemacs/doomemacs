;;; app/write/doctor.el -*- lexical-binding: t; -*-

(when (featurep! +langtool)
  (require 'langtool)
  (unless (file-exists-p langtool-language-tool-jar)
    (warn! "Couldn't find languagetool-commandline.jar")))
