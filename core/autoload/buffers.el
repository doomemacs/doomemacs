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
;;; Functions

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
  (let (buffer-list-update-hook)
    (get-buffer-create doom-fallback-buffer-name)))

;;;###autoload
(defalias 'doom-buffer-list #'buffer-list)

;;;###autoload
(defun doom-project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT.

If PROJECT is nil, default to the current project.

If no project is active, return all buffers."
  (let ((buffers (doom-buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (doom-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun doom-open-projects ()
  "Return a list of projects with open buffers."
  (cl-loop with projects = (make-hash-table :test 'equal :size 8)
           for buffer in (doom-buffer-list)
           if (buffer-live-p buffer)
           if (doom-real-buffer-p buffer)
           if (with-current-buffer buffer (doom-project-root))
           do (puthash (abbreviate-file-name it) t projects)
           finally return (hash-table-keys projects)))

;;;###autoload
(defun doom-dired-buffer-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun doom-special-buffer-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

;;;###autoload
(defun doom-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun doom-visible-buffer-p (buf)
  "Return non-nil if BUF is visible."
  (get-buffer-window buf))

;;;###autoload
(defun doom-buried-buffer-p (buf)
  "Return non-nil if BUF is not visible."
  (not (doom-visible-buffer-p buf)))

;;;###autoload
(defun doom-non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun doom-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `doom-real-buffer-p'."
  (cl-remove-if-not #'doom-real-buffer-p (or buffer-list (doom-buffer-list))))

;;;###autoload
(defun doom-real-buffer-p (buffer-or-name)
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
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (when-let (basebuf (buffer-base-buffer buf))
      (setq buf basebuf))
    (and (buffer-live-p buf)
         (not (doom-temp-buffer-p buf))
         (or (buffer-local-value 'doom-real-buffer-p buf)
             (run-hook-with-args-until-success 'doom-real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'doom-unreal-buffer-functions buf))))))

;;;###autoload
(defun doom-unreal-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an 'unreal' buffer.

See `doom-real-buffer-p' for details on what that means."
  (not (doom-real-buffer-p buffer-or-name)))

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
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

;;;###autoload
(defun doom-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

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
  "Forcibly mark BUFFER as FLAG (non-nil = real).

See `doom-real-buffer-p' for an explanation for real buffers."
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
(defun doom-fixup-windows (windows)
  "Ensure that each of WINDOWS is showing a real buffer or the fallback buffer."
  (dolist (window windows)
    (with-selected-window window
      (when (doom-unreal-buffer-p (window-buffer))
        (previous-buffer)
        (when (doom-unreal-buffer-p (window-buffer))
          (switch-to-buffer (doom-fallback-buffer)))))))

;;;###autoload
(defun doom-kill-buffer-fixup-windows (buffer)
  "Kill the BUFFER and ensure all the windows it was displayed in have switched
to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (kill-buffer buffer)
    (doom-fixup-windows (cl-remove-if-not #'window-live-p windows))))

;;;###autoload
(defun doom-kill-buffers-fixup-windows (buffers)
  "Kill the BUFFERS and ensure all the windows they were displayed in have
switched to a real buffer or the fallback buffer."
  (let ((seen-windows (make-hash-table :test 'eq :size 8)))
    (dolist (buffer buffers)
      (let ((windows (get-buffer-window-list buffer)))
        (kill-buffer buffer)
        (dolist (window (cl-remove-if-not #'window-live-p windows))
          (puthash window t seen-windows))))
    (doom-fixup-windows (hash-table-keys seen-windows))))

;;;###autoload
(defun doom-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (doom-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))


;;
;; Hooks

;;;###autoload
(defun doom-mark-buffer-as-real-h ()
  "Hook function that marks the current buffer as real.

See `doom-real-buffer-p' for an explanation for real buffers."
  (doom-set-buffer-real (current-buffer) t))


;;
;; Interactive commands

;;;###autoload
(defun doom/save-and-kill-buffer ()
  "Save the current buffer to file, then kill it."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

;;;###autoload
(defun doom/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer or the fallback buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))
  (doom-kill-buffer-fixup-windows buffer))


(defun doom--message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

;;;###autoload
(defun doom/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.

If the prefix arg is passed, doesn't close windows and only kill buffers that
belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (doom-project-buffer-list)
           (doom-buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (doom-fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (doom--message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun doom/kill-other-buffers (&optional buffer-list interactive)
  "Kill all other buffers (besides the current one).

If the prefix arg is passed, kill only buffers that belong to the current
project."
  (interactive
   (list (delq (current-buffer)
               (if current-prefix-arg
                   (doom-project-buffer-list)
                 (doom-buffer-list)))
         t))
  (mapc #'doom-kill-buffer-and-windows buffer-list)
  (doom--message-or-count
   interactive "Killed %d other buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun doom/kill-matching-buffers (pattern &optional buffer-list interactive)
  "Kill buffers that match PATTERN in BUFFER-LIST.

If the prefix arg is passed, only kill matching buffers in the current project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         (if current-prefix-arg
             (doom-project-buffer-list)
           (doom-buffer-list))
         t))
  (doom-kill-matching-buffers pattern buffer-list)
  (when interactive
    (message "Killed %d buffer(s)"
             (- (length buffer-list)
                (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun doom/kill-buried-buffers (&optional buffer-list interactive)
  "Kill buffers that are buried.

If PROJECT-P (universal argument), only kill buried buffers belonging to the
current project."
  (interactive
   (list (doom-buried-buffers
          (if current-prefix-arg (doom-project-buffer-list)))
         t))
  (mapc #'kill-buffer buffer-list)
  (doom--message-or-count
   interactive "Killed %d buried buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun doom/kill-project-buffers (project &optional interactive)
  "Kill buffers for the specified PROJECT."
  (interactive
   (list (if-let (open-projects (doom-open-projects))
             (completing-read
              "Kill buffers for project: " open-projects
              nil t nil nil
              (if-let* ((project-root (doom-project-root))
                        (project-root (abbreviate-file-name project-root))
                        ((member project-root open-projects)))
                  project-root))
           (message "No projects are open!")
           nil)
         t))
  (when project
    (let ((buffer-list (doom-project-buffer-list project)))
      (doom-kill-buffers-fixup-windows buffer-list)
      (doom--message-or-count
       interactive "Killed %d project buffers"
       (- (length buffer-list)
          (length (cl-remove-if-not #'buffer-live-p buffer-list)))))))
