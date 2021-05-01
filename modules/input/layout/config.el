;;; input/layout/config.el -*- lexical-binding: t; -*-

(add-hook! 'doom-init-modules-hook
  (defun +layout-init-h ()
    (cond ((featurep! +bepo)
           (load! "+bepo"))
          ((featurep! +azerty)
           (load! "+azerty")))))
