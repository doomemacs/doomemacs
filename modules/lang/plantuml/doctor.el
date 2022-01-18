;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/plantuml/doctor.el

(when (require 'plantuml-mode nil t)
  ;; java
  (unless (executable-find "java")
    (warn! "Couldn't find java. PlantUML preview or syntax checking won't work"))
  ;; graphviz
  (unless (executable-find "dot")
    (warn! "Couldn't fiind dot. PlantUML might not show any output"))
  ;; plantuml.jar
  (unless (file-exists-p plantuml-jar-path)
    (warn! "Couldn't find plantuml.jar. Install it with M-x plantuml-download-jar")))
