;;; tools/pass/autoload/consult.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion vertico)

;;;###autoload
(defun +pass/consult (arg pass)
  "TODO"
  (interactive
   (list current-prefix-arg
         (progn
           (require 'consult)
           (consult--read (password-store-list)
                          :prompt "Pass: "
                          :sort nil
                          :require-match t
                          :category 'pass))))
  (funcall (if arg
               #'password-store-url
             #'password-store-copy)
           pass))

;; TODO Add embark actions to +pass/consult
