;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(if (featurep! +switch-window)
    (package! switch-window :pin "8710f6304d843365fb59b6efe7e1f729d14e557c")
  (package! ace-window :pin "7003c88cd9cad58dc35c7cd13ebc61c355fb5be7"))

(when (featurep! +numbers)
  (package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
