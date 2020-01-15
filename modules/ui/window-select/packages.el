;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (featurep! +switch-window)
    (package! switch-window :pin "204f9fc1a39868a2d16ab9370a142c8c9c7a0943")
  (package! ace-window :pin "edbbb1b77c3fb939e4d9057443bc1897321d0095"))

(when (featurep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
