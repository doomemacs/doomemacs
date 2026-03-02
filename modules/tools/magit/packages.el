;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; HACK: Fixes magit/magit#5462. Remove when addressed upstream.
(defvar magit-auto-revert-mode nil)

;; REVIEW: This file contains pinned dependencies. This goes against our policy
;;   of only pinning primary packages, but an exception is made because the
;;   Magit ecosystem seems prone to breakage.

(package! transient :pin "1f7039ef8d548d6fe858084fcbeae7588eba4190") ; 0.12.0
(package! cond-let :pin "8bf87d45e169ebc091103b2aae325aece3aa804d") ; 0.2.2

(package! magit :pin "c800f79c2061621fde847f6a53129eca0e8da728") ; 4.5.0
(when (modulep! +forge)
  (package! closql :pin "947426d0c93e5ad5374c464b2f121c36cdaf2132") ; 2.4.0
  (package! forge
    :pin "315e8e9a2b45d050ca7fc717595cc698e175b140" ; 0.6.3
    ;; forge depends on ghub, which requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! ghub
    :pin "c22858596c1f5a1f5b439e475e7ba0e6a2e1718b" ; 5.0.4
    ;; ghub requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "303edcfbad8190eccb9a9269dfc58ed26d386ba5"
    ;; ...code-review depends on forge
    :disable (version< emacs-version "29.1")))

(when (modulep! :lang org)
  (package! orgit :pin "4fb91faff3bf32dac5f6f932654c280cd1f190f7") ; v2.1.2
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :pin "c2116b8701498bd11d8674065a5429d844985e46"))) ; v1.1.1
