;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (featurep! +switch-window)
    (package! switch-window :pin "204f9fc1a3")
  (package! ace-window :pin "7e0777b39a"))

(when (featurep! +numbers)
  (package! winum :pin "c5455e866e"))
