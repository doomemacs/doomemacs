;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el


(package! agda-input
  :recipe
  (:host github :repo "agda/agda"
         :files ("src/data/emacs-mode/agda-input.el")))

(package! agda2-mode
  :recipe
  (:host github :repo "agda/agda"
         :files
         ("src/data/emacs-mode/*.el"
          (:exclude "agda-input.el"))))
