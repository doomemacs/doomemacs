;;; lang/python/+conda.el -*- lexical-binding: t; -*-
;;;###if (featurep! +conda)

(def-package! conda
  :when (featurep! +conda)
  :after (python)
  :init
  (defvar +python-conda-home '("~/.anaconda3" "/usr/bin/anaconda3" "~/.anaconda")
    "A list of host pattern and corresponding anaconda home.")
  :config
  (advice-add 'anaconda-mode-bootstrap :override #'*anaconda-mode-bootstrap)
  (conda-env-autoactivate-mode -1)
  ;; (add-hook 'python-mode-hook #'conda-env-activate-for-buffer)
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  ;; Version management with conda
  (add-hook 'conda-postactivate-hook #'+python|add-version-to-modeline)
  (add-hook 'conda-postdeactivate-hook #'+python|add-version-to-modeline))
