;;; lang/common-lisp/doctor.el -*- lexical-binding: t; -*-

(when (require 'sly nil t)
  (let ((prog-name (car (split-string inferior-lisp-program))))
    (unless (executable-find prog-name)
      (warn! "Couldn't find your `inferior-lisp-program' (%s). Is it installed?"
             inferior-lisp-program))))
