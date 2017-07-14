;; -*- no-byte-compile: t; -*-
;;; lang/web/test/autoload-html.el

(def-test! encode-entities
  (should (equal (+web-encode-entities "Hello world")
                 "Hello world"))
  (should (equal (+web-encode-entities "H€llø wørld")
                 "H&euro;ll&oslash; w&oslash;rld")))

(def-test! decode-entities
  (should (equal (+web-decode-entities "Hello world")
                 "Hello world"))
  (should (equal (+web-decode-entities "H&euro;ll&oslash; w&oslash;rld")
                 "H€llø wørld")))
