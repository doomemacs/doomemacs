;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (featurep! +switch-window)
    (package! switch-window)
  (package! ace-window))

(when (featurep! +numbers)
  (package! winum))
