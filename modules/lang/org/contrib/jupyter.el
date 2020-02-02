;;; lang/org/contrib/jupyter.el -*- lexical-binding: t; -*-
;;;###if (featurep! +jupyter)

(defconst +org--company-box-icons-jupyter-alist '(("class"     . Class)
                                                  ("function"  . Function)
                                                  ("instance"  . Variable)
                                                  ("keyword"   . Keyword)
                                                  ("module"    . Module)
                                                  ("statement" . Variable)
                                                  ("param"     . Property)
                                                  ("path"      . File)))

(set-lookup-handlers! 'org-mode
  :documentation '+org/jupyter-documentation-lookup-handler)

(use-package! ob-jupyter
  :defer t
  :init
  (after! ob-async
    (pushnew! ob-async-no-async-languages-alist
              "jupyter-python"
              "jupyter-julia"
              "jupyter-R"))

  (after! org-src
    (dolist (lang '(python julia R))
      (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                  org-src-lang-modes :key #'car)))

  (add-hook! '+org-babel-load-functions
    (defun +org-babel-load-jupyter-h (lang)
      (when (string-prefix-p "jupyter-" (symbol-name lang))
        (require 'jupyter)
        (let* ((lang-name (symbol-name lang))
               (lang-tail (string-remove-prefix "jupyter-" lang-name)))
          (and (not (assoc lang-tail org-src-lang-modes))
               (require (intern (format "ob-%s" lang-tail))
                        nil t)
               (add-to-list 'org-src-lang-modes (cons lang-name (intern lang-tail)))))
        (with-demoted-errors "Jupyter: %s"
          (require lang nil t)
          (require 'ob-jupyter nil t)))))
  :config
  (advice-add 'org-babel-jupyter-initiate-session
              :after #'+org--ob-jupyter-initiate-session-a)
  ;; Remove text/html since it's not human readable
  (delq! :text/html jupyter-org-mime-types))

(after! company-box
  (push #'+org--company-box-icons-jupyter company-box-icons-functions))
