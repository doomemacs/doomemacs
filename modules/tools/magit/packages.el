;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! transient :pin "1d109f34b7c6af7af830c141bb0a3b4d33803e75") ; 0.10.0
(package! magit :pin "dc0094bd88a5307fdfa1c2a48f3ec5b33891f1f0") ; 4.4.0
(when (modulep! +forge)
  (package! forge
    :pin "bbecd8947a190894570b8344490a50ebf0efd394" ; 0.6.0
    ;; forge depends on ghub, which requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! ghub
    :pin "d76cdac69f9afb5c462c88bdc37c6192b43ac615" ; 5.0.0
    ;; ghub requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "303edcfbad8190eccb9a9269dfc58ed26d386ba5"
    ;; ...code-review depends on forge
    :disable (version< emacs-version "29.1")))
