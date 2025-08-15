;;; lang/common-lisp/doctor.el -*- lexical-binding: t; -*-

(when (require 'sly nil t)
  (unless (executable-find
           (car (if (listp inferior-lisp-program)
                    inferior-lisp-program
                  (split-string inferior-lisp-program))))
    (warn! "Couldn't find your `inferior-lisp-program' (%s). Is it installed?"
           inferior-lisp-program)))
