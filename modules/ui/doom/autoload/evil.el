;;; ui/doom/autoload/evil.el

;;;###autoload (autoload '+doom:scratch-buffer "ui/doom/autoload/evil" nil t)
(evil-define-operator +doom:scratch-buffer (&optional beg end bang)
  "Opens the scratch buffer in a popup window and, optionally, send the selected
region to it. If BANG, use current window instead of a popup."
  :move-point nil :type inclusive
  (interactive "<r><!>")
  (let ((text (when (and (evil-visual-state-p) beg end)
                (buffer-substring beg end)))
        (mode major-mode)
        (old-project (doom-project-root))
        (new-buf (get-buffer-create "*doom:scratch*")))
    (if bang
        (switch-to-buffer new-buf)
      (doom-popup-buffer new-buf))
    (with-current-buffer new-buf
      (setq default-directory old-project)
      (when (and (not (eq major-mode mode))
                 (functionp mode))
        (funcall mode))
      (if text (insert text)))))

