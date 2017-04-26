;;; ui/doom/autoload/evil.el

;;;###autoload (autoload '+doom:scratch-buffer "ui/doom/autoload/evil" nil t)
(evil-define-operator +doom:scratch-buffer (&optional beg end bang)
  "Send a region to and pop up the scratch buffer. If BANG, don't use a popup,
use the current window."
  :move-point nil :type inclusive
  ;; TODO Test me
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

