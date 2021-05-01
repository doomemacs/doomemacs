;;; input/layout/config.el -*- lexical-binding: t; -*-

(add-hook! 'doom-init-modules-hook
  (defun +layout-init-h ()
    (when (featurep! +bepo)
      (load! "+bepo"))))
