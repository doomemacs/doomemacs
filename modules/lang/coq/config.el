;;; lang/coq/config.el -*- lexical-binding: t; -*-

;; set this to the absolute path of the folder containing proof-site.el, e.g. /home/$USER/GitHub/PG/generic
(defvar +coq-pg-loc nil)

(def-package! proof-site
  :load-path +coq-pg-loc
  :defer t
  :mode ("\\.v\\'" . coq-mode)
  :hook (coq-mode . company-coq-mode))
