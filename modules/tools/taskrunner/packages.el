;; -*- no-byte-compile: t; -*-
;;; tools/taskrunner/packages.el

(package! taskrunner :pin "716323aff410b4d864d137c9ebe4bbb5b8587f5e")

(when (featurep! :completion helm)
  (package! helm-taskrunner
    :pin "70ef8117aafdc01a1f06151a82cecb9a2fcf4d32"
    :recipe (:host github :repo "emacs-taskrunner/helm-taskrunner")))

(when (featurep! :completion ivy)
  (package! ivy-taskrunner
    :pin "c731ee6279f65061ef70e79d3818ea1d9678e6da"
    :recipe (:host github :repo "emacs-taskrunner/ivy-taskrunner")))
