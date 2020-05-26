;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/magit/doctor.el

(assert! (or (not (featurep! +delta))
             (executable-find "delta"))
         "Couldn't find delta; magit-delta won't work")
