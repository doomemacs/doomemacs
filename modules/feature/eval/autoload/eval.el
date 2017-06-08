;;; feature/eval/autoload/eval.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eval/buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (cond ((assq major-mode +eval-runners-alist)
         (+eval/region (point-min) (point-max)))
        (t (quickrun))))

;;;###autoload
(defun +eval/region (beg end)
  "Evaluate a region and, if large enough, prints its output to a popup buffer (if an
elisp buffer). Otherwise forward the region to Quickrun."
  (interactive "r")
  (let ((load-file-name buffer-file-name))
    (if-let (runner (cdr (assq major-mode +eval-runners-alist)))
        (funcall runner beg end)
      (quickrun-region beg end))))

;;;###autoload
(defun +eval/region-and-replace (beg end)
  (interactive "r")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (kill-region beg end)
         (condition-case nil
             (prin1 (eval (read (current-kill 0)))
                    (current-buffer))
           (error (message "Invalid expression")
                  (insert (current-kill 0)))))
        (t (quickrun-replace-region beg end))))

