;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "a14671e03c867c9d759ee9e59cdc5cecbf271245")
(package! cider :pin "4278d7cf0b54af5cc84f4a521ee1ed6e81a96adc")
(package! clj-refactor :pin "9f3e7357117e96135de051b78deabc0a327c7b06")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
