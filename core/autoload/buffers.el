;;; autoload.el
(provide 'doom-lib-buffers)

(defvar-local doom-buffer--narrowed-origin nil)

;;;###autoload
(defvar doom-fallback-buffer "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

(defvar doom-buffers-unreal
  '("^ ?\\*" image-mode dired-mode reb-mode messages-buffer-mode
    tabulated-list-mode comint-mode magit-mode)
  "A list of regexps or modes whose buffers are considered unreal, and will be
ignored when using `doom:next-real-buffer' and `doom:previous-real-buffer' (or
killed by `doom:kill-old-buffers', or after `doom-kill-buffer').")

(defvar doom-buffers-processes-alist
  '(("pry" . ruby-mode)
    ("irb" . ruby-mode)
    ("ipython" . python-mode)
    ("tide" . typescript-mode))
  "An alist of (process-name . major-mode) that `doom-kill-process-buffers'
checks before killing processes. If there are no buffers with matching
major-modes, the process gets killed.")

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
(defun doom-buffer-list (&optional all-p)
  "Get all buffers in the current project, in the current workgroup.

If ALL-P is non-nil, get all buffers across all projects in current
workgroup."
  (let ((buffers (cond ((and (featurep 'workgroups2) workgroups-mode)
                        (wg-workgroup-associated-buffers nil))
                       ((and (featurep 'persp-mode) persp-mode)
                        (persp-buffer-list-restricted))
                       (t (buffer-list))))
        (project-root (and (not all-p) (doom-project-root t))))
    (append (if project-root
                (funcall (if (eq all-p 'not) '-remove '-filter)
                         (lambda (b) (projectile-project-buffer-p b project-root))
                         buffers)
              buffers)
            (list doom-fallback-buffer))))

;;;###autoload
(defun doom-real-buffers-list (&optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) that
`doom-real-buffer-p' returns non-nil for."
  (-filter #'doom-real-buffer-p (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-buffers-in-mode (modes &optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) whose
`major-mode' is one of MODES."
  (--filter (memq (buffer-local-value 'major-mode it) modes)
            (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-visible-windows (&optional window-list)
  "Get a list of the visible windows in the current frame (that aren't popups),
OR return only the visible windows in WINDOW-LIST."
  (-remove #'doom-popup-p (or window-list (window-list))))

;;;###autoload
(defun doom-visible-buffers (&optional buffer-list)
  "Get a list of unburied buffers in the current project and workgroup, OR
return only the unburied buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (-filter #'get-buffer-window (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-buried-buffers (&optional buffer-list)
  "Get a list of buried buffers in the current project and workgroup, OR return
only the buried buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (-remove 'get-buffer-window (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) that
match the regex PATTERN."
  (--filter (string-match-p pattern (buffer-name it))
            (or buffer-list (doom-buffer-list))))

(defun doom--cycle-real-buffers (&optional n)
  "Switch to the next buffer N times (previous, if N < 0), skipping over special
and unreal buffers. If there's nothing left, create a scratch buffer.

See `doom-real-buffer-p' for what 'real' means."
  (let* ((start-buffer (current-buffer))
         (move-func (if (> n 0) 'switch-to-next-buffer 'switch-to-prev-buffer))
         (max 25)
         (i 0)
         (buffers (doom-real-buffers-list))
         (fail-buffer (cond ((> (length (get-buffer-window-list doom-fallback-buffer nil t)) 1)
                             start-buffer)
                            ((bufferp doom-fallback-buffer)
                             doom-fallback-buffer)
                            (t doom-fallback-buffer)))
         destbuf)
    (setq destbuf
          (catch 'goto
            (if (or (not buffers)
                    (= (length buffers) 1))
                (progn (message "No other buffers in workspace")
                       (throw 'goto fail-buffer))
              (funcall move-func)
              (while (not (memq (current-buffer) buffers))
                (if (or (eq (current-buffer) start-buffer)
                        (>= i max))
                    (throw 'goto fail-buffer)
                  (funcall move-func))
                (cl-incf i))
              (current-buffer))))
    (when (eq destbuf fail-buffer)
      (message "Nowhere to go"))
    (switch-to-buffer destbuf)))

;;;###autoload
(defun doom-real-buffer-p (&optional buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer. Real means:

a) it isn't a popup (or temporary) window
b) it isn't a special buffer (e.g. scratch or *messages* buffer)
c) and its major-mode or buffer-name-matching regexp isn't in
`doom-buffers-unreal'."
  (let ((buffer (window-normalize-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (not (or (doom-popup-p (get-buffer-window buffer))
               (eq (buffer-name buffer) doom-fallback-buffer)
               (--any? (and (stringp it) (string-match-p it (buffer-name buffer)))
                       doom-buffers-unreal)
               (with-current-buffer buffer
                 (apply 'derived-mode-p (-filter 'symbolp doom-buffers-unreal))))))))

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
buffer, but buries the buffer if it is present in another window.

See `doom-real-buffer-p' for what 'real' means."
  (let* ((old-project (doom-project-root))
         (buffer (or buffer (current-buffer)))
         (buffer-win (get-buffer-window buffer))
         (only-buffer-window-p (= (length (get-buffer-window-list buffer nil t)) 1)))
    (with-current-buffer buffer
      (when (and only-buffer-window-p
                 (buffer-file-name buffer)
                 (buffer-modified-p buffer))
        (if (and (not dont-save)
                 (yes-or-no-p "Buffer is unsaved, save it?"))
            (save-buffer)
          (set-buffer-modified-p nil)))
      (if (window-dedicated-p buffer-win)
          (unless (window--delete buffer-win t t)
            (split-window buffer-win)
            (window--delete buffer-win t t))
        (doom--cycle-real-buffers -1)
        (unless (eq (current-buffer) buffer)
          (when only-buffer-window-p
            (kill-buffer buffer)
            (unless (doom-real-buffer-p)
              (doom--cycle-real-buffers -1))))
        (when buffer-win
          (unrecord-window-buffer buffer-win buffer))
        (eq (current-buffer) buffer)))))

;;;###autoload
(defun doom-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (unless (one-window-p t)
    (mapc (lambda (win) (unless (one-window-p t) (delete-window win)))
          (get-buffer-window-list buf))))

;;;###autoload
(defun doom-kill-process-buffers ()
  "Kill all buried buffers that represent running processes."
  (interactive)
  (let ((buffer-list (buffer-list))
        (n 0))
    (dolist (p (process-list))
      (let* ((process-name (process-name p))
             (assoc (assoc process-name doom-buffers-processes-alist)))
        (when (and assoc
                   (not (string= process-name "server"))
                   (process-live-p p)
                   (not (--any? (let ((mode (buffer-local-value 'major-mode it)))
                                  (eq mode (cdr assoc)))
                                buffer-list)))
          (delete-process p)
          (setq n (1+ n)))))
    n))

;;;###autoload
(defun doom-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workgroup OR in BUFFER-LIST) that match the
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
  (let ((buffers (doom-buffer-list (not project-p))))
    (mapc 'doom-kill-buffer-and-windows buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun doom/kill-other-buffers (&optional project-p)
  "Kill all other buffers in this workgroup. If PROJECT-P, kill only the other
buffers that belong to the current project."
  (interactive "P")
  (let ((buffers (doom-buffer-list (not project-p))))
    (mapc (lambda (buf)
            (unless (eq buf (current-buffer))
              (doom-kill-buffer-and-windows buf)))
          buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun doom/kill-matching-buffers (pattern &optional project-p)
  "Kill buffers in current workgroup that match regex PATTERN. If BANG, then
exclude buffers that aren't part of the current project."
  (interactive "sP")
  (let* ((buffers (doom-buffer-list (not project-p)))
         (n (doom-kill-matching-buffers pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" n))))

;;;###autoload
(defun doom/cleanup-buffers ()
  "Clean up buried, unreal buffers."
  (interactive)
  (let ((buffers (doom-buried-buffers (doom-buffer-list t))))
    (mapc 'doom-kill-buffer buffers)
    (setq n (+ (doom-kill-process-buffers) (length buffers)))
    (when (called-interactively-p 'interactive)
      (message "Cleaned up %s buffers" n))))

