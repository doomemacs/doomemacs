;; -*- no-byte-compile: t; -*-
;;; core/test/test-core-modules.el

(xdescribe "core-modules"
  (require 'core-modules)

  (describe "doom!")
  (describe "doom-modules")

  (describe "doom-module-p")
  (describe "doom-module-get")
  (describe "doom-module-put")
  (describe "doom-module-set")
  (describe "doom-module-path")
  (describe "doom-module-locate-path")
  (describe "doom-module-from-path")
  (describe "doom-module-load-path")

  (describe "require!")
  (describe "featurep!")
  (describe "after!")
  (describe "use-package!")
  (describe "use-package-hook!"))
