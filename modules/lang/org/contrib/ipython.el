;;; lang/org/contrib/babel.el -*- lexical-binding: t; -*-
;;;###if (featurep! +ipython)

(def-package! ob-ipython
  :defer t
  :init
  (defvar +ob-ipython-local-runtime-dir nil)

  (setq ob-ipython-resources-dir ".ob-ipython-resrc")

  (defun +org|babel-load-ipython (lang)
    (and (string-prefix-p "jupyter-" (symbol-name lang))
         (require 'ob-ipython nil t)))
  (add-hook '+org-babel-load-functions #'+org|babel-load-ipython)
  :config
  (set-popup-rules!
    '(("\\*ob-ipython.*"
       :slot 2 :side right :size 100 :height 0.2
       :select nil :quit nil :transient nil)
      ("^ ?\\*Python"
       :slot 0 :side right :size 100
       :select nil :quit nil :ttl nil)))

  ;; advices for remote kernel and org-src-edit
  (advice-add #'ob-ipython--create-repl :override #'+org*ob-ipython--create-repl)
  (advice-add #'org-babel-edit-prep:ipython :override #'+org*babel-edit-prep:ipython)
  (advice-add #'org-babel-execute:ipython :override #'+org*babel-execute:ipython)
  (advice-add #'org-babel-ipython-initiate-session :override #'+org*ob-ipython-initiate-session)

  ;; retina resolution image hack
  (when IS-MAC
    (advice-add #'ob-ipython--write-base64-string :around #'+org*ob-ipython--write-base64-string))

  ;; ipython has its own async keyword, disable ipython in ob-async.
  (after! ob-async
    (add-to-list 'ob-async-no-async-languages-alist "ipython")))
