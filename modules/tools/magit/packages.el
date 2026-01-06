;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; HACK: Fixes magit/magit#5462. Remove when addressed upstream.
(defvar magit-auto-revert-mode nil)

;; REVIEW: This file contains pinned dependencies. This goes against our policy
;;   of only pinning primary packages, but an exception is made because the
;;   Magit ecosystem seems prone to breakage.

(package! transient :pin "1f7039ef8d548d6fe858084fcbeae7588eba4190") ; 0.12.0
(package! cond-let :pin "0430bd1eb3493ea90d69feb6b7eb7dac3e10d0ba") ; 0.2.1

(package! magit :pin "c800f79c2061621fde847f6a53129eca0e8da728") ; 4.5.0
(when (modulep! +forge)
  (package! closql :pin "947426d0c93e5ad5374c464b2f121c36cdaf2132") ; 2.4.0
  (package! forge
    :pin "315e8e9a2b45d050ca7fc717595cc698e175b140" ; 0.6.3
    ;; forge depends on ghub, which requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! ghub
    :pin "278d9fb5f3f673a4ecfe551faeacc0dfd5de0783" ; 5.0.3
    ;; ghub requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "303edcfbad8190eccb9a9269dfc58ed26d386ba5"
    ;; ...code-review depends on forge
    :disable (version< emacs-version "29.1")))
