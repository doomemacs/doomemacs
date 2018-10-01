;;; lang/ruby/autoload.el -*- lexical-binding: t; -*-

(defvar +ruby-version-cache (make-hash-table :test 'equal)
  "TODO")

;;;###autoload
(defun +ruby|cleanup-robe-servers ()
  "Clean up dangling inf robe processes if there are no more `enh-ruby-mode'
buffers open."
  ;; FIXME This should wait X seconds before cleaning up
  (unless (or (not robe-mode) (doom-buffers-in-mode 'enh-ruby-mode))
    (let (inf-buffer kill-buffer-query-functions)
      (while (setq inf-buffer (robe-inf-buffer))
        (let ((process (get-buffer-process inf-buffer))
              confirm-kill-processes)
          (when (processp process)
            (kill-process (get-buffer-process inf-buffer))
            (kill-buffer inf-buffer)))))))

;;;###autoload
(defun +ruby-version ()
  "Return the currently installed version of ruby on your system (the first
ruby executable found in your PATH).

This is not necessarily aware of env management tools like virtualenv, pyenv or
pipenv, unless those tools have modified the PATH that Emacs picked up when you
started it."
  (condition-case _
      (let ((version-str (car (process-lines "ruby" "--version"))))
        (puthash (or (doom-project-root) default-directory)
                 (format "Ruby %s" (cadr (split-string version-str " ")))
                 +ruby-version-cache))
    (error "Ruby")))


;;
;; Hooks

;;;###autoload
(defun +ruby|update-version (&rest _)
  "Update `+ruby--version' by consulting `+ruby-version' function."
  (setq +ruby--version
        (or (gethash (or (doom-project-root) default-directory)
                     +ruby-version-cache)
            (+ruby-version))))

;;;###autoload
(defun +ruby|update-version-in-all-buffers (&rest )
  "Update `+ruby--version' in all `enh-ruby-mode' buffers."
  (dolist (buffer (doom-buffers-in-mode 'enh-ruby-mode))
    (setq +ruby-version-cache (clrhash +ruby-version-cache))
    (with-current-buffer buffer
      (+ruby|update-version))))
