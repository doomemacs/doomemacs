;; -*- no-byte-compile: t; -*-
;;; tools/taskrunner/packages.el

(package! taskrunner :pin "716323aff")

(when (featurep! :completion helm)
  (package! helm-taskrunner
    :recipe (:host github :repo "emacs-taskrunner/helm-taskrunner")
    :pin "70ef8117aa"))

(when (featurep! :completion ivy)
  (package! ivy-taskrunner
    :recipe (:host github :repo "emacs-taskrunner/ivy-taskrunner")
    :pin "c731ee6279"))
