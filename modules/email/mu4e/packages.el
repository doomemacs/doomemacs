;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(package! mu4e-compat
  :recipe (:host github :repo "tecosaur/mu4e-compat")
   :pin "a33345cb8ef83554f01510bbc8f5c7323713aa8d")
(when (modulep! +org)
  (package! org-msg :pin "327768e2c38020f6ea44730e71f2a62f3f0ce3bd"))
