;;; lang/org/contrib/jupyter.el -*- lexical-binding: t; -*-
;;;###if (featurep! +jupyter)

(use-package! ob-jupyter
  :defer t
  :init
  (after! ob-async
    (pushnew! ob-async-no-async-languages-alist "jupyter-python" "jupyter-julia"))

  (add-hook! '+org-babel-load-functions
    (defun +org-babel-load-jupyter-h (lang)
      (when (string-prefix-p "jupyter-" (symbol-name lang))
        (let ((lang (string-remove-prefix "jupyter-" (symbol-name lang))))
          (unless (assoc lang org-src-lang-modes)
            (require (intern (format "ob-%s" lang))
                     nil t)))
        (with-demoted-errors "Jupyter: %s"
          (require lang nil t)
          (require 'ob-jupyter nil t))))))
