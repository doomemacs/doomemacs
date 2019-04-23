;;; lang/python/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "python")
  (warn! "Python isn't installed."))

(when (featurep! +pyenv)
  (if (not (executable-find "pyenv"))
      (warn! "Couldn't find pyenv in your PATH")
    (unless (split-string (shell-command-to-string "pyenv versions --bare") "\n" t)
      (warn! "No versions of python are available via pyenv, did you forget to install one?"))))

(when (featurep! +conda)
  (unless (executable-find "conda")
    (warn! "Couldn't find conda in your PATH")))

(when (featurep! +ipython)
  (unless (executable-find "ipython")
    (warn! "Couldn't find ipython in your PATH")))
