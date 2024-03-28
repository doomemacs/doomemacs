;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/ebuild/doctor.el

(unless (executable-find "pkgdev")
  (warn! "Couldn't find pkgdev. Pkgdev commands (ebuild-mode-run-pkgdev) won't work"))

(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (unless (executable-find "pkgcheck")
    (warn! "Couldn't find pkgcheck. Ebuild check won't work")))

(unless (modulep! :lang sh)
  (warn! "This module requires (:lang sh)"))
