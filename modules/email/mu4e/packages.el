;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (modulep! +org)
  (package! org-msg :pin "59e2042e5f23e25f31c6aef0db1e70c6f54f117d"))
