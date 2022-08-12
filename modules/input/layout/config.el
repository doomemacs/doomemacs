;;; input/layout/config.el -*- lexical-binding: t; -*-

(add-hook! 'doom-init-modules-hook
  (defun +layout-init-h ()
    (cond ((modulep! +bepo)
           (load! "+bepo"))
          ((modulep! +azerty)
           (load! "+azerty")))))
