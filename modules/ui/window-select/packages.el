;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (featurep! +switch-window)
    (package! switch-window :pin "2801d9b3a9d7bf0b64cd8b2f2e572124ed2ff9ad")
  (package! ace-window :pin "c7cb315c14e36fded5ac4096e158497ae974bec9"))

(when (featurep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
