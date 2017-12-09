;;; org/org/test/org.el -*- lexical-binding: t; -*-

(when (featurep 'org) (unload-feature 'org t))
(require! :org org)
(require 'org (locate-library "org" nil doom--package-load-path))
