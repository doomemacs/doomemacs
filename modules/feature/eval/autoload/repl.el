;;; feature/repl/autoload/repl.el

;;;###autoload
(defun +repl/open ()
  (interactive)
  ;; TODO
  (error "Not implemented"))

;;;###autoload
(defun +repl/send ()
  (interactive)
  ;; TODO
  (error "Not implemented"))

;;;###autoload (autoload '+repl:open "feature/repl/autoload/repl" nil t)
;;;###autoload (autoload '+repl:send "feature/repl/autoload/repl" nil t)

(after! evil
  (evil-define-command +repl:open (&optional bang command)
    :repeat nil
    (interactive "<!><a>")
    (if (and doom-repl-buffer (buffer-live-p doom-repl-buffer))
        (doom-popup-buffer doom-repl-buffer)
      (rtog/toggle-repl (if (use-region-p) 4))
      (setq doom-repl-buffer (current-buffer))
      (when command
        (with-current-buffer doom-repl-buffer
          (insert command)
          (unless bang (comint-send-input))))))

  (evil-define-operator +repl:eval (&optional beg end bang)
    :type inclusive :repeat nil
    (interactive "<r><!>")
    (let ((region-p (use-region-p))
          (selection (s-trim (buffer-substring-no-properties beg end))))
      (doom:repl bang)
      (when (and region-p beg end)
        (let* ((buf doom-repl-buffer)
               (win (get-buffer-window buf)))
          (unless (eq buf (doom-popup-p (get-buffer-window buf)))
            (doom-popup-buffer buf))
          (when (and doom-repl-buffer (buffer-live-p doom-repl-buffer))
            (with-current-buffer doom-repl-buffer
              (goto-char (point-max))
              (insert selection))))))))
