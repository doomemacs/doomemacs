;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (featurep! +switch-window)
    (package! switch-window :pin "8d9fe251d8d38b223d643df975876356ddfc1b98")
  (package! ace-window :pin "0577c426a9833ab107bab46c60d1885c611b2fb9"))

(when (featurep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
