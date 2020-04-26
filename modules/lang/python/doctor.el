;;; lang/python/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(if (not (executable-find "python"))
    (error! "Python isn't installed.")
  (unless (featurep! +lsp)
    (unless (zerop (shell-command "python -c 'import setuptools'"))
      (warn! "setuptools wasn't detected, which anaconda-mode requires"))))

(when (featurep! +pyenv)
  (if (not (executable-find "pyenv"))
      (warn! "Couldn't find pyenv in your PATH")
    (unless (split-string (shell-command-to-string "pyenv versions --bare") "\n" t)
      (warn! "No versions of python are available via pyenv, did you forget to install one?"))))

(when (featurep! +conda)
  (unless (executable-find "conda")
    (warn! "Couldn't find conda in your PATH")))

(when (featurep! +poetry)
  (if (not (executable-find "poetry"))
      (warn! "Couldn't find poetry in your PATH")))

(when (featurep! +cython)
  (unless (executable-find "cython")
    (warn! "Couldn't find cython. cython-mode will not work.")))

(when (featurep! +ipython)
  (unless (executable-find "ipython")
    (warn! "Couldn't find ipython in your PATH")))

(unless (executable-find "pytest")
  (warn! "Couldn't find pytest. Running tests through pytest will not work."))

(unless (executable-find "nosetests")
  (warn! "Couldn't find nosetests. Running tests through nose will not work."))

(unless (executable-find "pipenv")
  (warn! "Couldn't find pipenv. pipenv support will not work."))

(unless (executable-find "isort")
  (warn! "Couldn't find isort. Import sorting will not work."))

(when (featurep! :editor format)
  (unless (executable-find "pyflakes")
    (warn! "Couldn't find pyflakes. Import management will not work."))
  (unless (executable-find "black")
    (warn! "Couldn't find black. Code formatting will not work.")))
