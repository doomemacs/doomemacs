;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; HACK: Fixes magit/magit#5462. Remove when addressed upstream.
(defvar magit-auto-revert-mode nil)

(package! transient :pin "053d56e4de2dd78bf32f7af7ed5f289a91cdb6ac") ; 0.10.1
(package! magit :pin "b828afbb4b45641998fb6483a08effb1efb214e1") ; 4.4.2
(when (modulep! +forge)
  (package! forge
    :pin "71910a26e360bfe88eb81b47f377f7694161fe9b" ; 0.6.2
    ;; forge depends on ghub, which requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! ghub
    :pin "447cb51fa7d19e1fb3844acdd2c540be04299ffb" ; 5.0.2
    ;; ghub requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "303edcfbad8190eccb9a9269dfc58ed26d386ba5"
    ;; ...code-review depends on forge
    :disable (version< emacs-version "29.1")))
