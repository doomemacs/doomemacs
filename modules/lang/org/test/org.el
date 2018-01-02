;;; lang/org/test/org.el -*- lexical-binding: t; -*-

(when (featurep 'org)
  (unload-feature 'org t))
(require! :lang org)

(require 'org (locate-library "org" nil doom--package-load-path))

;;
