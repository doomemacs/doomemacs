;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (modulep! +switch-window)
    (package! switch-window :pin "71ef2f54c97f3fd2e7ff7964d82e6562eb6282f7")
  (package! ace-window :pin "77115afc1b0b9f633084cf7479c767988106c196"))

(when (modulep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
