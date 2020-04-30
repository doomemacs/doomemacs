;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/java/doctor.el

(assert! (or (not (featurep! +google-java-format))
             (featurep! :editor format))
         "This module requires (:editor format)")

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "javac")
  (warn! "Couldn't find the javac executable, are you sure the JDK is installed?"))

(when (and (featurep! +google-java-format)
           (not (executable-find "google-java-format")))
  (warn! "Couldn't find google-java-format. Code-formatting will be unavailable"))
