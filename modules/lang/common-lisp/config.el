;;; lang/common-lisp/config.el -*- lexical-binding: t; -*-

;; `slime'
(setq slime-contribs '(slime-fancy))
(after! slime
  (setq inferior-lisp-program (executable-find "sbcl")))


(def-package! slime-company
  :when (featurep! :completion company)
  :commands slime-company
  :init
  (push 'slime-company slime-contribs)

  ;; Don't override the global `company-backends'!
  (defun +common-lisp|make-company-backends-local (&rest _)
    (make-variable-buffer-local 'company-backends))
  (advice-add #'slime-company-maybe-enable
              :before #'+common-lisp|make-company-backends-local))

