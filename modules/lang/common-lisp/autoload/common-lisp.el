;;; lang/common-lisp/autoload/common-lisp.el -*- lexical-binding: t; -*-

;; HACK Fix #1772: void-variable sly-contribs errors due to sly packages (like
;; `sly-macrostep') trying to add to `sly-contribs' before it is defined.
;;;###autoload (defvar sly-contribs '(sly-fancy))

;;;###autoload
(defun +lisp/open-repl ()
  "Open the Sly REPL."
  (interactive)
  (require 'sly)
  (if (sly-connected-p) (sly-mrepl)
    (sly nil nil t)
    (cl-labels ((recurse (attempt)
                  (sleep-for 1)
                  (cond ((sly-connected-p) (sly-mrepl))
                        ((> attempt 5) (error "Failed to start Slynk process."))
                        (t (recurse (1+ attempt))))))
      (recurse 1))))

;;;###autoload
(defun +lisp/reload-project ()
  "Restart the Sly session and reload a chosen system."
  (interactive)
  (sly-restart-inferior-lisp)
  (cl-labels ((recurse (attempt)
                (sleep-for 1)
                (condition-case nil
                    (sly-eval "PONG")
                  (error (if (= 5 attempt)
                             (error "Failed to reload Lisp project in 5 attempts.")
                           (recurse (1+ attempt)))))))
    (recurse 1)
    (sly-asdf-load-system)))

;;;###autoload
(defun +lisp/find-file-in-quicklisp ()
  "Find a file belonging to a library downloaded by Quicklisp."
  (interactive)
  (doom-project-find-file "~/.quicklisp/dists/"))
