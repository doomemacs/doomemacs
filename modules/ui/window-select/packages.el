;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (modulep! +switch-window)
    (package! switch-window :pin "8f771b571a1e60fac2d2a9845c0a5a52d5b440df")
  (package! ace-window :pin "77115afc1b0b9f633084cf7479c767988106c196"))

(when (modulep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
