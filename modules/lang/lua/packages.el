;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode)

(when (featurep! +moonscript)
  (package! moonscript)
  (when (featurep! :checkers syntax)
    (package! flycheck-moonscript
      :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript"))))

(when (featurep! :completion company)
  (package! company-lua))

