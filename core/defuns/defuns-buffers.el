;;; defuns-buffers.el

(defvar doom-buffer)
(defvar-local doom--narrowed-origin nil)

;;;###autoload (autoload 'doom:narrow "defuns-buffers" nil t)
(evil-define-operator doom:narrow (&optional beg end bang)
  "Restrict editing in this buffer to the current region, indirectly. With BANG,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "<r><!>")
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (when bang
          (let ((old-buf (current-buffer)))
            (switch-to-buffer (clone-indirect-buffer nil nil))
            (setq doom--narrowed-origin old-buf)))
        (narrow-to-region beg end))
    (if doom--narrowed-origin
        (progn
          (kill-this-buffer)
          (switch-to-buffer doom--narrowed-origin)
          (setq doom--narrowed-origin nil))
      (widen))))

;;;###autoload
(defun doom/set-read-only-region (begin end)
  "Mark a region as read-only (http://stackoverflow.com/questions/7410125)"
  (let ((modified (buffer-modified-p)))
    (add-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun doom/set-region-writeable (begin end)
  "Undoes `doom/set-read-only-region' (http://stackoverflow.com/questions/7410125)"
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))


;; Buffer Life and Death ;;;;;;;;;;;;;;;

;;;###autoload
(defun doom/get-buffers (&optional all-p)
  "Get all buffers in the current project, in the current workgroup.

    If ALL-P is non-nil, get all buffers across all projects in current
workgroup."
  (let ((buffers (if (wg-current-workgroup t)
                     (doom/get-buffers-in-workgroup)
                   (buffer-list)))
        (project-root (and (not all-p) (doom/project-root t))))
    (append (if project-root
                (funcall (if (eq all-p 'not) '-remove '-filter)
                         (lambda (b) (projectile-project-buffer-p b project-root))
                         buffers)
              buffers)
            (list doom-buffer))))

;;;###autoload
(defun doom/get-buffers-in-workgroup ()
  "Get a list of buffers in current workgroup. Returns nil if workgroups2 isn't
loaded."
  (when (featurep 'workgroups2)
    (let ((assoc-bufs (wg-workgroup-associated-buffers nil)))
      (--filter (memq it assoc-bufs) (buffer-list)))))

;;;###autoload
(defun doom/get-buffer-names (&optional buffer-list)
  "Get a list of names of buffers in the current workgroup, OR return the names
of the buffers in BUFFER-LIST."
  (mapcar #'buffer-name (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-visible-windows (&optional window-list)
  "Get a list of the visible windows in the current frame (that aren't popups),
OR return only the visible windows in WINDOW-LIST."
  (-remove #'doom/popup-p (or window-list (window-list))))

;;;###autoload
(defun doom/get-visible-buffers (&optional buffer-list)
  "Get a list of unburied buffers in the current project and workgroup, OR
return only the unburied buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (-filter #'get-buffer-window (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-buried-buffers (&optional buffer-list)
  "Get a list of buried buffers in the current project and workgroup, OR return
only the buried buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (-remove 'get-buffer-window (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) that
match the regex PATTERN."
  (--filter (string-match-p pattern (buffer-name it))
            (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-buffers-in-modes (modes &optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) whose
`major-mode' is one of MODES."
  (--filter (memq (buffer-local-value 'major-mode it) modes)
            (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-real-buffers (&optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) that
`doom/real-buffer-p' returns non-nil for."
  (-filter #'doom/real-buffer-p (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/kill-real-buffer (&optional arg)
  "Kill buffer then switch to a real buffer. Only buries the buffer if it is
being displayed in another window.

NOTE: only buries scratch buffer.

See `doom/real-buffer-p' for what 'real' means."
  (interactive (list t))
  (let* ((scratch-p (doom-scratch-buffer-p))
         (old-project (doom/project-root))
         (buffer (current-buffer))
         (only-buffer-window-p (= (length (get-buffer-window-list buffer nil t)) 1)))
    (unless scratch-p
      (when (and buffer-file-name only-buffer-window-p (buffer-modified-p))
        (if (yes-or-no-p "Buffer is unsaved, save it?")
            (save-buffer)
          (set-buffer-modified-p nil)))
      (when arg
        (doom/previous-real-buffer)
        (unless (eq (current-buffer) buffer)
          (when only-buffer-window-p
            (kill-buffer buffer)
            (unless (doom/real-buffer-p)
              (doom/previous-real-buffer)))))))
  (when (doom-scratch-buffer-p)
    (doom-scratch-force-reload))
  t)

;;;###autoload
(defun doom/kill-unreal-buffers ()
  "Kill all buried buffers in current frame that match any of the rules in
`doom-unreal-buffers'."
  (interactive)
  (let ((kill-list (-remove 'doom/real-buffer-p
                            (doom/get-buried-buffers (buffer-list)))))
    (mapc 'kill-buffer kill-list)
    (doom/kill-process-buffers)
    (message "Cleaned up %s buffers" (length kill-list))))

;;;###autoload
(defun doom/kill-process-buffers ()
  "Kill all buried buffers that represent running processes."
  (interactive)
  (let ((buffer-list (buffer-list))
        (killed-processes 0))
    (dolist (p (process-list))
      (let* ((process-name (process-name p))
             (assoc (assoc process-name doom-cleanup-processes-alist)))
        (when (and assoc
                   (not (string= process-name "server"))
                   (process-live-p p)
                   (not (--any? (let ((mode (buffer-local-value 'major-mode it)))
                                  (eq mode (cdr assoc)))
                                buffer-list)))
          (delete-process p)
          (incf killed-processes))))
    (message "Cleaned up %s processes" killed-processes)))

;;;###autoload
(defun doom/kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workgroup OR in BUFFER-LIST) that match the
regex PATTERN."
  (interactive)
  (let ((i 0))
    (mapc (lambda (b)
            (when (string-match-p pattern (buffer-name b))
              (kill-buffer b)
              (setq i (1+ i))))
          (if buffer-list buffer-list (doom/get-buffers)))
    (message "Killed %s matches" i)))

;;;###autoload
(defun doom/cycle-real-buffers (&optional n)
  "Switch to the previous buffer, skipping over special buffers. If there's
nothing left, create a scratch buffer."
  (let* ((start-buffer (current-buffer))
         (move-func (if (> n 0) 'switch-to-next-buffer 'switch-to-prev-buffer))
         (max 25)
         (i 0)
         (continue t)
         (buffers (doom/get-real-buffers (doom/get-buffers t)))
         (fail-buffer (if (> (length (get-buffer-window-list doom-buffer nil t)) 1)
                          start-buffer
                        doom-buffer))
         destbuf)
    (setq destbuf
          (catch 'goto
            (if (or (not buffers)
                    (= (length buffers) 1))
                (progn (message "No other buffers in workgroup")
                       (throw 'goto fail-buffer))
              (funcall move-func)
              (while (not (memq (current-buffer) buffers))
                (if (or (eq (current-buffer) start-buffer)
                        (>= i max))
                    (throw 'goto fail-buffer)
                  (funcall move-func))
                (cl-incf i))
              (current-buffer))))
    (when (eq destbuf doom-buffer)
      (doom-scratch-reload)
      (message "Nowhere to go"))
    (switch-to-buffer destbuf)))

;;;###autoload
(defun doom/real-buffer-p (&optional buffer)
  "Returns whether BUFFER a 'real' buffer or not. Real means: a) it isn't a
popup (or temporary) window and b) it isn't a special buffer (e.g. scratch or
*messages* buffer)."
  (setq buffer (or (and (bufferp buffer) buffer)
                   (and (stringp buffer) (get-buffer buffer))
                   (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (not (or (apply #'derived-mode-p (-filter 'symbolp doom-unreal-buffers))
               (--any? (string-match-p it (buffer-name buffer))
                       (-filter 'stringp doom-unreal-buffers)))))))

;;;###autoload
(defun doom/next-real-buffer ()
  "Switch to the next real buffer, skipping special buffers. See
`doom/real-buffer-p'."
  (interactive)
  (doom/cycle-real-buffers +1))

;;;###autoload
(defun doom/previous-real-buffer ()
  "Switch to the previous real buffer, skipping special buffers. See
`doom/real-buffer-p'."
  (interactive)
  (doom/cycle-real-buffers -1))

(defun doom--kill-buffers (buffers &optional filter-func)
  (let ((buffers (if filter-func (-filter filter-func buffers) buffers))
        (affected 0))
    (mapc (lambda (b) (when (kill-buffer b) (incf affected))) buffers)
    (unless (doom/real-buffer-p)
      (doom/previous-real-buffer))
    (message "Killed %s buffers" affected)))

;;;###autoload (autoload 'doom:kill-all-buffers "defuns-buffers" nil t)
(evil-define-command doom:kill-all-buffers (&optional bang)
  "Kill all project buffers. If BANG, kill *all* buffers (in workgroup)."
  (interactive "<!>")
  (doom--kill-buffers (--filter (not (eq it doom-buffer))
                                (doom/get-buffers (not bang))))
  (delete-other-windows)
  (switch-to-buffer doom-buffer))

;;;###autoload (autoload 'doom:kill-other-buffers "defuns-buffers" nil t)
(evil-define-command doom:kill-other-buffers (&optional bang)
  "Kill all other project buffers. If BANG, kill *all* other buffers (in workgroup)."
  (interactive "<!>")
  (doom--kill-buffers (doom/get-buffers (not bang))
                      (lambda (b) (not (eq b (current-buffer)))))
  (when bang
    (delete-other-windows)))

;;;###autoload (autoload 'doom:kill-buried-buffers "defuns-buffers" nil t)
(evil-define-command doom:kill-buried-buffers (&optional bang)
  "Kill buried project buffers in current workgroup and report how many it
found. If BANG, then include buffers that aren't part of the current project."
  (interactive "<!>")
  (doom--kill-buffers (doom/get-buried-buffers (doom/get-buffers (not bang)))))

;;;###autoload (autoload 'doom:kill-buried-buffers "defuns-buffers" nil t)
(evil-define-command doom:kill-matching-buffers (&optional bang pattern)
  "Kill project buffers in current workgroup that match regex PATTERN. If BANG,
then include buffers that aren't part of the current project."
  :repeat nil
  (interactive "<!><a>")
  (doom--kill-buffers (doom/get-matching-buffers pattern (doom/get-buffers (not bang)))))

;;;###autoload (autoload 'doom:scratch-buffer "defuns-buffers" nil t)
(evil-define-operator doom:scratch-buffer (&optional beg end bang)
  "Send a region to and pop up the scratch buffer. If BANG, don't use a popup
(use the current window)."
  :move-point nil
  :type inclusive
  (interactive "<r><!>")
  (let ((text (when (and (evil-visual-state-p) beg end)
                (buffer-substring beg end)))
        (mode major-mode)
        (old-project (doom/project-root))
        (new-buf (get-buffer-create "*doom:scratch*")))
    (with-current-buffer new-buf
      (setq default-directory old-project)
      (setq mode-line-format (doom-modeline))
      (when (and (not (eq major-mode mode))
                 (functionp mode))
        (funcall mode))
      (if text (insert text)))
    (if bang (switch-to-buffer new-buf) (doom/popup-buffer new-buf))))

;;;###autoload (autoload 'doom:cd "defuns-buffers" nil t)
(evil-define-command doom:cd (dir)
  "Change the `default-directory' to DIR (alias for `cd')"
  :repeat nil
  (interactive "<f>")
  (cd (if (zerop (length dir)) "~" dir)))

;;;###autoload
(defun doom/kill-workgroup-and-quit ()
  "Wipe the current workgroup session and save the blank slate."
  (interactive)
  (let (confirm-kill-emacs)
    (mapc 'kill-buffer (doom/get-buffers t))
    (kill-this-buffer)
    (delete-other-windows)
    (wg-save-session t)
    (save-buffers-kill-terminal)))

(provide 'defuns-buffers)
;;; defuns-buffers.el ends here
