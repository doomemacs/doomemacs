;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! transient :pin "aa32e0d66cc389befed7a8e8df9439d92a729daa") ; 0.9.4
(package! magit :pin "5b820a1d1e94649e0f218362286d520d9f29ac2c") ; 4.3.8
(when (modulep! +forge)
  (package! forge
    :pin "a31859547a1ea5e2acbab67b6b64f90134e2a156" ; 0.5.3
    ;; forge depends on ghub, which requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! ghub
    :pin "97a07691efad6fc16bc000a35be80d4f8dae251a" ; 4.3.2
    ;; ghub requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "303edcfbad8190eccb9a9269dfc58ed26d386ba5"
    ;; ...code-review depends on forge
    :disable (version< emacs-version "29.1")))
