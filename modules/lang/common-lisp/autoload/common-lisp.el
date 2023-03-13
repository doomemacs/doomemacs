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
(defun +lisp/load-project-systems ()
  "Load all systems of the current Lisp project into Sly."
  (interactive)
  (thread-last (+lisp--project-asd-file)
               (+lisp--systems-from-asd)
               (mapcar (lambda (s) (format ":%s" s)))
               (funcall (lambda (ss) (string-join ss " ")))
               (format "(ql:quickload '(%s))")
               (sly-interactive-eval)))

(defun +lisp--project-asd-file ()
  "Yield an absolute file path to the current project's `.asd' file."
  (let* ((proot (doom-project-root))
         (files (doom-files-in proot :depth 1 :match "[.]asd$")))
    (pcase files
      ('() (error "No .asd file found in: %s" proot))
      (`(,asdf) asdf)
      (_ (error "Too many .asd files found in : %s" proot)))))

(defun +lisp--systems-from-asd (asdf)
  "Given a path to an ASDF project definition, extract the names of
the systems defined therein."
  (let ((file (doom-file-read asdf))
        (patt "defsystem \"\\([a-z-/]+\\)"))
    (when (not (string-match patt file))
      (error "No systems defined in: %s" asdf))
    (thread-last (s-match-strings-all patt file)
                 (mapcar #'cl-second))))

;; TODO Get this to run in a comint buffer?
;;;###autoload
(defun +lisp/test-system ()
  "Run `asdf:test-system' on the selected system of the current project."
  (interactive)
  (thread-last (+lisp--project-asd-file)
               (+lisp--systems-from-asd)
               (completing-read "Test which Lisp system?")
               (format "(asdf:test-system :%s)")
               (sly-interactive-eval)))

;;;###autoload
(defun +lisp/reload-project ()
  "Restart the Sly session and reload the current project."
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
    (+lisp/load-project-systems)))

;;;###autoload
(defun +lisp/find-file-in-quicklisp ()
  "Find a file belonging to a library downloaded by Quicklisp."
  (interactive)
  (doom-project-find-file "~/.quicklisp/dists/"))
