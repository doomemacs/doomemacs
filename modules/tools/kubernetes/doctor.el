;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/kubernetes/doctor.el

(unless (executable-find "kubectl")
  (warn! "Couldn't find kubectl. kubernetes-overview will not work"))
