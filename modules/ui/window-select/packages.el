;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (featurep! +switch-window)
    (package! switch-window :pin "5bba78073bc18197392f4d86eb1fe284f906219c")
  (package! ace-window :pin "c7cb315c14e36fded5ac4096e158497ae974bec9"))

(when (featurep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
