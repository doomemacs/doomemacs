;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; app/write/doctor.el

(when (featurep! +langtool)
  (require 'langtool)
  (unless (file-exists-p langtool-language-tool-jar)
    (warn! "Couldn't find languagetool-commandline.jar")))
