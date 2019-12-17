;;; lang/org/contrib/jupyter.el -*- lexical-binding: t; -*-
;;;###if (featurep! +jupyter)

(use-package! ob-jupyter
  :defer t
  :init
  (after! ob-async
    (pushnew! ob-async-no-async-languages-alist "jupyter-python" "jupyter-julia"))

  (add-hook! '+org-babel-load-functions
    (defun +org-babel-load-jupyter-h (lang)
      (and (string-prefix-p "jupyter-" (symbol-name lang))
           (require 'ob-jupyter nil t)))))
