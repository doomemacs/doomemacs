;;; feature/repl/autoload/eval.el

;;;###autoload
(defun +repl/eval-buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (+repl/eval-region (point-min) (point-max)))
        (t (quickrun))))

;;;###autoload
(defun +repl/eval-region (beg end)
  "Evaluate a region and, if large enough, prints its output to a popup buffer (if an
elisp buffer). Otherwise forward the region to Quickrun."
  (interactive "r")
  (let ((load-file-name buffer-file-name))
    (cond ((eq major-mode 'emacs-lisp-mode)
           (require 'pp)
           (let ((result (eval (read (buffer-substring-no-properties beg end))))
                 lines)
             (let ((buf (get-buffer-create "*eval*")))
               (with-current-buffer buf
                 ;; FIXME messy!
                 (read-only-mode -1)
                 (setq-local scroll-margin 0)
                 (erase-buffer)
                 (prin1 result buf)
                 (emacs-lisp-mode)
                 (pp-buffer)
                 (read-only-mode 1)
                 (setq lines (count-lines (point-min) (point-max)))
                 (goto-char (point-min))
                 (when (< lines 5)
                   (message "%s" (buffer-substring (point-min) (point-max)))
                   (kill-buffer buf)))
               (unless (< lines 5)
                 (doom-popup-buffer buf)))))
          (t (quickrun-region beg end)))))

;;;###autoload
(defun +repl/eval-region-and-replace (beg end)
  (interactive "r")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (kill-region beg end)
         (condition-case nil
             (prin1 (eval (read (current-kill 0)))
                    (current-buffer))
           (error (message "Invalid expression")
                  (insert (current-kill 0)))))
        (t (quickrun-replace-region beg end))))

