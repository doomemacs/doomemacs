;;; lang/ruby/autoload.el -*- lexical-binding: t; -*-

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
  (unless (executable-find "ruby")
    (user-error "Couldn't find ruby executable in PATH"))
  (with-temp-buffer
    (let ((p (call-process "ruby" nil (current-buffer) nil "--version"))
          (output (string-trim (buffer-string))))
      (unless (zerop p)
        (user-error "ruby --version failed: %s" output))
      (nth 1 (split-string output " " t)))))
