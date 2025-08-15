;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(package! mu4e-compat
  :recipe (:host github :repo "tecosaur/mu4e-compat")
   :pin "a33345cb8ef83554f01510bbc8f5c7323713aa8d")
(when (modulep! +org)
  (package! org-msg :pin "59e2042e5f23e25f31c6aef0db1e70c6f54f117d"))
