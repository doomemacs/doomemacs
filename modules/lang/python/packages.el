;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

(package! anaconda-mode)
(package! nose)
(package! pip-requirements)

(when (featurep! :completion company)
  (package! company-anaconda))

;;
(def-bootstrap! python
  ;; Since there are so many possible setups for a python environment (pyenv,
  ;; virtualenv, etc), I'll leave it to you and only take care of installing
  ;; dependencies available via pip.
  (unless (executable-find "python")
    (error "python isn't installed"))
  (unless (executable-find "pip")
    (error "pip isn't installed"))
  (when-let (pkgs (cl-remove-if
                   (lambda (pkg) (zerop (shell-command (format "pip show %s" pkg))))
                   '("jedi" "setuptools")))
    (funcall (if (file-writable-p (executable-find "pip")) 'sh 'sudo)
             "pip install %s"
             (string-join pkgs " "))
    t))
