;;; lang/common-lisp/autoload/common-lisp.el -*- lexical-binding: t; -*-

;; HACK Fix #1772: void-variable sly-contribs errors due to sly packages (like
;; `sly-macrostep') trying to add to `sly-contribs' before it is defined.
;;;###autoload (defvar sly-contribs '(sly-fancy))

;;;###autoload
(defun +common-lisp--sly-last-sexp-a (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))
