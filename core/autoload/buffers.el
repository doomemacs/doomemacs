;;; core/autoload/buffers.el -*- lexical-binding: t; -*-

(defvar-local doom-buffer--narrowed-origin nil)

;;;###autoload
(defvar doom-real-buffer-functions '()
  "A list of predicate functions run to determine if a buffer is real. These
functions are iterated over with one argument, the buffer in question. If any
function returns non-nil, the procession stops and the buffer is qualified as
real.")

;;;###autoload
(defvar-local doom-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what.")

;;;###autoload
(defvar doom-fallback-buffer "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")


;;
;; Functions
;;

;;;###autoload
(defun doom-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer."
  (get-buffer-create doom-fallback-buffer))

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
(defun doom-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `doom-real-buffer-p'."
  (cl-loop for buf in (or buffer-list (doom-buffer-list))
           if (doom-real-buffer-p buf)
           collect buf))

;;;###autoload
(defun doom-real-buffer-p (&optional buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer. The complete criteria for a
real buffer is:

  1. The buffer-local value of `doom-real-buffer-p' (variable) is non-nil OR
  2. Any function in `doom-real-buffer-functions' must return non-nil when
     passed this buffer OR
  3. The current buffer:
     a) has a `buffer-file-name' defined AND
     b) is not in a popup window (see `doom-popup-p') AND
     c) is not a special buffer (its name isn't something like *Help*)

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (when-let* ((buf (ignore-errors (window-normalize-buffer buffer-or-name))))
    (or (buffer-local-value 'doom-real-buffer-p buf)
        (run-hook-with-args-until-success 'doom-real-buffer-functions buf)
        (not (or (doom-popup-p buf)
                 (minibufferp buf)
                 (string-match-p "^\\s-*\\*" (buffer-name buf))
                 (not (buffer-file-name buf)))))))

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
  (cl-loop for win in (or window-list (window-list))
           unless (doom-popup-p win)
           collect win))

;;;###autoload
(defun doom-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (cl-loop for buf in (or buffer-list (doom-buffer-list))
           when (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun doom-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-loop for buf in (or buffer-list (doom-buffer-list))
           unless (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun doom-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (doom-buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

(defun doom--cycle-real-buffers (&optional n)
  "Switch to the next buffer N times (previous, if N < 0), skipping over unreal
buffers. If there's nothing left, switch to `doom-fallback-buffer'. See
`doom-real-buffer-p' for what 'real' means."
  (let ((buffers (delq (current-buffer) (doom-real-buffer-list))))
    (cond ((or (not buffers)
               (zerop (% n (1+ (length buffers)))))
           (switch-to-buffer (doom-fallback-buffer) nil t))
          ((= (length buffers) 1)
           (switch-to-buffer (car buffers) nil t))
          (t
           ;; Why this instead of switching straight to the Nth buffer in
           ;; BUFFERS? Because `switch-to-next-buffer' and
           ;; `switch-to-prev-buffer' properly update buffer list order.
           (cl-loop with move-func =
                    (if (> n 0) #'switch-to-next-buffer #'switch-to-prev-buffer)
                    for i to 20
                    while (not (memq (current-buffer) buffers))
                    do
                    (dotimes (_i (abs n))
                      (funcall move-func)))))
    (force-mode-line-update)
    (current-buffer)))

;;;###autoload
(defun doom-set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq doom-real-buffer-p flag)))

;;;###autoload
(defun doom-kill-buffer (&optional buffer dont-save)
  "Kill BUFFER (defaults to current buffer), but make sure we land on a real
buffer. Bury the buffer if the buffer is present in another window.

Will prompt to save unsaved buffers when attempting to kill them, unless
DONT-SAVE is non-nil.

See `doom-real-buffer-p' for what 'real' means."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (and (bufferp buffer)
             (buffer-live-p buffer))
    (let ((buffer-win (get-buffer-window buffer)))
      ;; deal with modified buffers
      (when (and (buffer-file-name buffer)
                 (buffer-modified-p buffer))
        (with-current-buffer buffer
          (if (and (not dont-save)
                   (yes-or-no-p "Buffer is unsaved, save it?"))
              (save-buffer)
            (set-buffer-modified-p nil))))
      ;; kill the buffer (or close dedicated window)
      (cond ((not buffer-win)
             (kill-buffer buffer))
            ((window-dedicated-p buffer-win)
             (unless (window--delete buffer-win t t)
               (split-window buffer-win)
               (window--delete buffer-win t t)))
            (t ; cycle to a real buffer
             (with-selected-window buffer-win
               (doom--cycle-real-buffers -1)
               (kill-buffer buffer)))))
    (not (eq (current-buffer) buffer))))

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
      (doom-kill-buffer buf t))))


;;
;; Interactive commands
;;

;;;###autoload
(defun doom/kill-this-buffer (&optional interactive-p)
  "Use `doom-kill-buffer' on the current buffer."
  (interactive (list 'interactive))
  (when (and (not (doom-kill-buffer)) interactive-p)
    (message "Nowhere left to go!")))

;;;###autoload
(defun doom/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (let ((windows (get-buffer-window-list buffer nil t)))
    (doom-kill-buffer buffer dont-save)
    (cl-loop for win in windows
             if (doom-real-buffer-p (window-buffer win))
             do (with-selected-window win (doom/previous-buffer)))))

;;;###autoload
(defun doom/kill-all-buffers (&optional project-p)
  "Kill all buffers and closes their windows.

If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (doom/popup-kill-all)
  (let ((buffers (if project-p (doom-project-buffer-list) (doom-buffer-list))))
    (mapc #'doom-kill-buffer-and-windows buffers)
    (unless (doom-real-buffer-p)
      (switch-to-buffer (doom-fallback-buffer)))
    (message "Killed %s buffers" (length buffers))))

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
  (interactive "P")
  (run-hooks 'doom-cleanup-hook)
  (let ((buffers (doom-buried-buffers (if all-p (buffer-list))))
        (n 0)
        kill-buffer-query-functions)
    (mapc #'kill-buffer buffers)
    (setq n (+ n (length buffers) (doom/cleanup-processes)))
    (when (called-interactively-p 'interactive)
      (message "Cleaned up %s buffers" n))))

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

;;;###autoload
(defun doom/next-buffer ()
  "Switch to the next real buffer, skipping non-real buffers. See
`doom-real-buffer-p' for what 'real' means."
  (interactive)
  (doom--cycle-real-buffers +1))

;;;###autoload
(defun doom/previous-buffer ()
  "Switch to the previous real buffer, skipping non-real buffers. See
`doom-real-buffer-p' for what 'real' means."
  (interactive)
  (doom--cycle-real-buffers -1))
