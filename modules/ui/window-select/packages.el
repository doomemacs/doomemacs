;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (modulep! +switch-window)
    (package! switch-window :pin "a72cf11d21c1f24924a9faeaa9f5d213d8623141")
  (package! ace-window :pin "77115afc1b0b9f633084cf7479c767988106c196"))

(when (modulep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
