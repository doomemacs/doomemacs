;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! gptel :pin "1aa5f1c10df34aeb08f677cd4aa4ef1fc12e87df")

(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "34acd437a7af8a387c14428bd1abdb3cd9e95d9d")

(when (modulep! :tools magit)
  (package! gptel-magit :pin "7f586943040bbb6885adafaf3e61fb5137c64558"))
