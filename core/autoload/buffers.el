;;; autoload.el

(defvar-local doom-buffer--narrowed-origin nil)

;;;###autoload
(defvar doom-real-buffer-functions '()
  "A list of functions that are run to determine if a buffer is real.")

;;;###autoload
(defvar doom-fallback-buffer "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

;;;###autoload
(defun doom-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer."
  (get-buffer-create doom-fallback-buffer))

;;;###autoload
(defun doom-narrow-buffer (beg end &optional clone-p)
  "Restrict editing in this buffer to the current region, indirectly. With CLONE-P,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "r")
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (when clone-p
          (let ((old-buf (current-buffer)))
            (switch-to-buffer (clone-indirect-buffer nil nil))
            (setq doom-buffer--narrowed-origin old-buf)))
        (narrow-to-region beg end))
    (if doom-buffer--narrowed-origin
        (progn
          (kill-this-buffer)
          (switch-to-buffer doom-buffer--narrowed-origin)
          (setq doom-buffer--narrowed-origin nil))
      (widen))))


;; Buffer Life and Death ;;;;;;;;;;;;;;;
;;;###autoload
(defun doom-buffer-list (&optional project-p)
  "Get all buffers in the current project, in the current workspace.

If PROJECT-P is non-nil, get all buffers associated with the current project in
the current workspace."
  (let ((buffers (if (and (featurep 'persp-mode) persp-mode)
                     (persp-buffer-list-restricted)
                   (buffer-list)))
        (project-root (and project-p (doom-project-root t))))
    (if project-root
        (funcall (if (eq project-p 'not) 'cl-remove-if 'cl-remove-if-not)
                 (lambda (b) (projectile-project-buffer-p b project-root))
                 buffers)
      buffers)))

;;;###autoload
(defun doom-real-buffers-list (&optional buffer-list)
  "Get a list of all buffers (in the current workspace OR in BUFFER-LIST) that
`doom-real-buffer-p' returns non-nil for."
  (cl-remove-if-not 'doom-real-buffer-p (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-buffers-in-mode (modes &optional buffer-list)
  "Get a list of all buffers (in the current workspace OR in BUFFER-LIST) whose
`major-mode' is one of MODES."
  (let ((modes (if (listp modes) modes (list modes))))
    (cl-remove-if-not (lambda (buf) (memq (buffer-local-value 'major-mode buf) modes))
                      (or buffer-list (doom-buffer-list)))))

;;;###autoload
(defun doom-visible-windows (&optional window-list)
  "Get a list of the visible windows in the current frame (that aren't popups),
OR return only the visible windows in WINDOW-LIST."
  (cl-remove-if 'doom-popup-p (or window-list (window-list))))

;;;###autoload
(defun doom-visible-buffers (&optional buffer-list)
  "Get a list of unburied buffers in the current project and workspace, OR
return only the unburied buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (cl-remove-if-not 'get-buffer-window (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-buried-buffers (&optional buffer-list)
  "Get a list of buried buffers in the current project and workspace, OR return
only the buried buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (cl-remove-if 'get-buffer-window (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers (in the current workspace OR in BUFFER-LIST) that
match the regex PATTERN."
  (cl-remove-if-not (lambda (buf) (string-match-p pattern (buffer-name buf)))
                    (or buffer-list (doom-buffer-list))))

(defun doom--cycle-real-buffers (&optional n)
  "Switch to the next buffer N times (previous, if N < 0), skipping over unreal
buffers. If there's nothing left, switch to `doom-fallback-buffer'. See
`doom-real-buffer-p' for what 'real' means."
  (let ((buffers (delq (current-buffer) (doom-real-buffers-list)))
        (project-dir (doom-project-root)))
    (cond ((or (not buffers)
               (zerop (% n (1+ (length buffers)))))
           (set-window-buffer nil (doom-fallback-buffer)))
          ((= (length buffers) 1)
           (set-window-buffer nil (car buffers)))
          (t
           (let ((move-func (if (> n 0) 'switch-to-next-buffer 'switch-to-prev-buffer)))
             ;; Why this instead of switching straight to the Nth buffer in
             ;; BUFFERS? Because `switch-to-next-buffer' and
             ;; `switch-to-prev-buffer' properly update buffer list order.
             (while (not (memq (current-buffer) buffers))
               (dotimes (i (abs n))
                 (funcall move-func))))))
    (when (eq (current-buffer) (doom-fallback-buffer))
      (cd project-dir))
    (current-buffer)))

;;;###autoload
(defun doom-real-buffer-p (&optional buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer. Real means it a) isn't a
popup window/buffer and b) isn't a special buffer."
  (let ((buf (window-normalize-buffer buffer-or-name)))
    (or (run-hook-with-args-until-success 'doom-real-buffer-functions buf)
        (not (or (doom-popup-p buf)
                 (minibufferp buf)
                 (string-match-p "^\\s-*\\*" (buffer-name buf)))))))

;;;###autoload
(defun doom/next-buffer ()
  "Switch to the next real buffer, skipping special buffers. See
`doom-real-buffer-p' for what 'real' means."
  (interactive)
  (doom--cycle-real-buffers +1))

;;;###autoload
(defun doom/previous-buffer ()
  "Switch to the previous real buffer, skipping special buffers. See
`doom-real-buffer-p' for what 'real' means."
  (interactive)
  (doom--cycle-real-buffers -1))

;;;###autoload
(defun doom-kill-buffer (&optional buffer dont-save)
  "Kill BUFFER (falls back to current buffer if omitted) then switch to a real
buffer, but only bury the buffer if it is present in another window.

See `doom-real-buffer-p' for what 'real' means."
  (let* ((buffer (or buffer (current-buffer)))
         (buffer-win (get-buffer-window buffer))
         (only-buffer-window-p (= 1 (length (get-buffer-window-list buffer nil t)))))
    (when (and only-buffer-window-p
               (buffer-file-name buffer)
               (buffer-modified-p buffer))
      (with-current-buffer buffer
        (if (and (not dont-save)
                 (yes-or-no-p "Buffer is unsaved, save it?"))
            (save-buffer)
          (set-buffer-modified-p nil))))
    (if (window-dedicated-p buffer-win)
        (unless (window--delete buffer-win t t)
          (split-window buffer-win)
          (window--delete buffer-win t t))
      (doom--cycle-real-buffers -1)
      (when buffer-win
        (unrecord-window-buffer buffer-win buffer))
      (when only-buffer-window-p
        (kill-buffer buffer)))
    (eq (current-buffer) buffer)))

;;;###autoload
(defun doom-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (unless (one-window-p t)
    (mapc (lambda (win) (unless (one-window-p t) (delete-window win)))
          (get-buffer-window-list buffer)))
  (kill-buffer buffer))

;;;###autoload
(defun doom-kill-process-buffers ()
  "Kill all processes that have no visible associated buffers."
  (interactive)
  (let ((n 0))
    (dolist (p (process-list))
      (let ((process-buffer (process-buffer p)))
        (when (and (process-live-p p)
                   (not (string= (process-name p) "server"))
                   (or (not process-buffer)
                       (and (bufferp process-buffer)
                            (not (buffer-live-p process-buffer)))))
          (message "Killing %s" (process-name p))
          (delete-process p)
          (setq n (1+ n)))))
    n))

;;;###autoload
(defun doom-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (doom-matching-buffers pattern buffer-list)))
    (mapc 'doom-kill-buffer buffers)
    (length buffers)))

;;;###autoload
(defun doom/kill-this-buffer ()
  "Uses `doom-kill-buffer' on the current buffer."
  (interactive)
  (when (and (doom-kill-buffer) (called-interactively-p 'interactive))
    (message "Nowhere left to go!")))

;;;###autoload
(defun doom/kill-all-buffers (&optional project-p)
  "Kill all buffers in this workspace. If PROJECT-P, kill all buffers that
belong to the current project in this workspace."
  (interactive "P")
  (let ((buffers (doom-buffer-list project-p)))
    (mapc 'doom-kill-buffer-and-windows buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun doom/kill-other-buffers (&optional project-p)
  "Kill all other buffers in this workspace. If PROJECT-P, kill only the other
buffers that belong to the current project."
  (interactive "P")
  (let ((buffers (doom-buffer-list project-p)))
    (mapc (lambda (buf)
            (unless (eq buf (current-buffer))
              (doom-kill-buffer-and-windows buf)))
          buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun doom/kill-matching-buffers (pattern &optional project-p)
  "Kill buffers in current workspace that match regex PATTERN. If BANG, then
exclude buffers that aren't part of the current project."
  (interactive "sP")
  (let* ((buffers (doom-buffer-list project-p))
         (n (doom-kill-matching-buffers pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" n))))

;;;###autoload
(defun doom/cleanup-buffers ()
  "Clean up buried and process buffers in the current workspace."
  (interactive)
  (let ((buffers (doom-buried-buffers)))
    (mapc 'kill-buffer buffers)
    (setq n (+ (doom-kill-process-buffers) (length buffers)))
    (when (called-interactively-p 'interactive)
      (message "Cleaned up %s buffers" n))))

