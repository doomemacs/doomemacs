;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg)
(package! merlin)
(package! merlin-eldoc)
(package! ocp-indent)

(when (featurep! :feature syntax-checker)
  (package! flycheck-ocaml))

(when (featurep! :feature eval)
  (package! utop))

(when (featurep! :editor format)
  ;; by default quelpa generated a version 0pre0.20180929.192844, which got
  ;; parsed into (0 -1 0 ...), which when compared with version nil (0) in
  ;; package-installed-p always yielded false
  (package! ocamlformat :recipe (:fetcher github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))))

(package! dune :recipe (:fetcher github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el")))


;; (defvar +ocaml-elisp-dir
;;   (when (executable-find "opam")
;;     (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share" "--safe")))))
;;       (when (and opam-share (file-directory-p opam-share))
;;         (expand-file-name "emacs/site-lisp" opam-share)))))
;;
;; (defmacro localpackage! (name)
;;   `(package! ,name :recipe (:fetcher file :path ,+ocaml-elisp-dir)))
;;
;; (localpackage! opam-site-lisp)
