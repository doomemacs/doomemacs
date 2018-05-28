;;; lang/org/+ipython.el -*- lexical-binding: t; -*-
(defvar +ob-ipython-local-runtime-dir
  (substring (shell-command-to-string (concat "jupyter --runtime-dir")) 0 -1))

(def-package! ob-ipython
  :when (featurep! +ipython)
  :after (org ob)
  :config (setq ob-ipython-resources-dir ".ob-ipython-resrc/")
  ;; popup
  (set!
    :popup "^\\*Org Src"
    '((size . 100)
      (side . right)
      (slot . -1)
      (window-height . 0.6))
    '((quit)
      (select . t)
      (modeline)))
  (set!
    :popup "^\\*Python"
    '((slot . 0)
      (side . right)
      (size . 100))
    '((select) (quit) (transient)))
  (set!
    :popup "\\*ob-ipython.*"
    '((slot . 2)
      (side . right)
      (size . 100)
      (window-height . 0.2))
    '((select) (quit) (transient)))
  (set!
    :popup "\\*Python:.*"
    '((slot . 0)
      (side . right)
      (size . 100))
    '((select) (quit) (transient)))
  ;; advices for remote kernel and org-src-edit
  (advice-add 'org-babel-edit-prep:ipython :override #'*org-babel-edit-prep:ipython)
  (advice-add 'org-babel-ipython-initiate-session :override #'*org-babel-ipython-initiate-session)
  (advice-add 'ob-ipython--create-repl :override #'*ob-ipython--create-repl)
  (advice-add 'org-babel-execute:ipython :override #'*org-babel-execute:ipython)
  ;; retina resolution image hack
  (when (eq window-system 'ns)
    (advice-add 'ob-ipython--write-base64-string :around #'*ob-ipython--write-base64-string)))
