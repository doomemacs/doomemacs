;;; lang/common-lisp/autoload/common-lisp.el -*- lexical-binding: t; -*-

;; HACK Fix #1772: void-variable sly-contribs errors due to sly packages (like
;; `sly-macrostep') trying to add to `sly-contribs' before it is defined.
;;;###autoload (defvar sly-contribs '(sly-fancy))
