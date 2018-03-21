;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/plantuml/doctor.el

(when (require 'plantuml-mode nil t)
  ;; java
  (unless (executable-find "java")
    (warn! "Couldn't find java. PlantUML preview or syntax checking won't work"))
  ;; plantuml.jar
  (unless (file-exists-p plantuml-jar-path)
    (warn! "Couldn't find plantuml.jar. Install it with-x +plantuml/install")))
