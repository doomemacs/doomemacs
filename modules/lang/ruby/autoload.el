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
