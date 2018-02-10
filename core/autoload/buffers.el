;;; core/autoload/buffers.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar doom-real-buffer-functions
  '(doom-dired-buffer-p)
  "A list of predicate functions run to determine if a buffer is real, unlike
`doom-unreal-buffer-functions'. They are passed one argument: the buffer to be
tested.

Should any of its function returns non-nil, the rest of the functions are
ignored and the buffer is considered real.

See `doom-real-buffer-p' for more information.")

;;;###autoload
(defvar doom-unreal-buffer-functions
  '(minibufferp doom-special-buffer-p doom-non-file-visiting-buffer-p)
  "A list of predicate functions run to determine if a buffer is *not* real,
unlike `doom-real-buffer-functions'. They are passed one argument: the buffer to
be tested.

Should any of these functions return non-nil, the rest of the functions are
ignored and the buffer is considered unreal.

See `doom-real-buffer-p' for more information.")

;;;###autoload
(defvar-local doom-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what. See
`doom-real-buffer-p' for more information.")

;;;###autoload
(defvar doom-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")


;;
;; Functions
;;

;;;###autoload
(defun doom-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (doom-real-buffer-p buf)
      (eq buf (doom-fallback-buffer))))

;;;###autoload
(defun doom-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `doom-fallback-buffer-name' to change this."
  (get-buffer-create doom-fallback-buffer-name))

;;;###autoload
(defalias 'doom-buffer-list #'buffer-list)

;;;###autoload
(defun doom-project-buffer-list ()
  "Return a list of buffers belonging to the current project.

If no project is active, return all buffers."
  (let ((buffers (doom-buffer-list)))
    (if-let* ((project-root (if (doom-project-p) (doom-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun doom-dired-buffer-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun doom-special-buffer-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (string-match-p "^\\s-*\\*" (buffer-name buf)))

;;;###autoload
(defun doom-non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun doom-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `doom-real-buffer-p'."
  (cl-remove-if-not #'doom-real-buffer-p (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-real-buffer-p (&optional buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.

A real buffer is a useful buffer; a first class citizen in Doom. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.

The exact criteria for a real buffer is:

  1. A non-nil value for the buffer-local value of the `doom-real-buffer-p'
     variable OR
  2. Any function in `doom-real-buffer-functions' returns non-nil OR
  3. None of the functions in `doom-unreal-buffer-functions' must return
     non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (when-let* ((buf (ignore-errors (window-normalize-buffer buffer-or-name))))
    (or (buffer-local-value 'doom-real-buffer-p buf)
        (run-hook-with-args-until-success 'doom-real-buffer-functions buf)
        (not (run-hook-with-args-until-success 'doom-unreal-buffer-functions buf)))))

;;;###autoload
(defun doom-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (doom-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (doom-buffer-list)))))

;;;###autoload
(defun doom-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for window in (or window-list (window-list))
           unless (eq (window-dedicated-p window) 'side)
           collect window))

;;;###autoload
(defun doom-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (cl-loop for buf in (or buffer-list (doom-buffer-list))
           when (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun doom-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-remove-if #'get-buffer-window (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (doom-buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun doom-set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq doom-real-buffer-p flag)))

;;;###autoload
(defun doom-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun doom-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (doom-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))


;;
;; Interactive commands
;;

;;;###autoload
(defun doom/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (let ((windows (get-buffer-window-list buffer nil t)))
    (when (and (buffer-modified-p buffer) dont-save)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)))
    (kill-buffer buffer)
    (cl-loop for win in windows
             if (doom-real-buffer-p (window-buffer win))
             do (with-selected-window win (previous-buffer)))))

;;;###autoload
(defun doom/kill-all-buffers (&optional project-p)
  "Kill all buffers and closes their windows.

If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (let ((buffers (if project-p (doom-project-buffer-list) (doom-buffer-list)))
        (ignore-window-parameters t))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer))
    (let (kill-buffer-query-functions)
      (message "Killed %s buffers"
               (length (delq nil (mapcar #'kill-buffer buffers)))))))

;;;###autoload
(defun doom/kill-other-buffers (&optional project-p)
  "Kill all other buffers (besides the current one).

If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (let ((buffers (if project-p (doom-project-buffer-list) (doom-buffer-list)))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (doom-kill-buffer-and-windows buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun doom/kill-matching-buffers (pattern &optional project-p)
  "Kill buffers that match PATTERN in BUFFER-LIST.

If PROJECT-P (universal argument), only kill matching buffers in the current
project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         current-prefix-arg))
  (let* ((buffers (if project-p (doom-project-buffer-list) (doom-buffer-list)))
         (n (doom-kill-matching-buffers pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" n))))

;;;###autoload
(defun doom/cleanup-session (&optional all-p)
  "Clean up buried buries and orphaned processes in the current workspace. If
ALL-P (universal argument), clean them up globally."
  (interactive (list current-prefix-arg))
  (let ((buffers (doom-buried-buffers (if all-p (buffer-list))))
        (n 0))
    (mapc #'kill-buffer buffers)
    (setq n (+ n (length buffers) (doom/cleanup-processes)))
    (dolist (hook doom-cleanup-hook)
      (let ((m (funcall hook)))
        (when (integerp m)
          (setq n (+ n m)))))
    (message "Cleaned up %s buffers" n)
    n))

;;;###autoload
(defun doom/cleanup-processes ()
  "Kill all processes that have no visible associated buffers. Return number of
processes killed."
  (interactive)
  (let ((n 0))
    (dolist (p (process-list))
      (let ((process-buffer (process-buffer p)))
        (when (and (process-live-p p)
                   (not (string= (process-name p) "server"))
                   (or (not process-buffer)
                       (and (bufferp process-buffer)
                            (not (buffer-live-p process-buffer)))))
          (delete-process p)
          (cl-incf n))))
    n))
