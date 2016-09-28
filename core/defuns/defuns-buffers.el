;;; defuns-buffers.el

;;;###autoload (autoload 'doom:narrow "defuns-buffers" nil t)
(evil-define-operator doom:narrow (&optional beg end bang)
  "Restrict editing in this buffer to the current region, indirectly. With BANG,
clone the buffer and hard-narrow the selection. Otherwise use fancy-narrow. If
mark isn't active, then widen the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "<r><!>")
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (let ((buf (clone-indirect-buffer nil nil)))
          (with-current-buffer buf
            (narrow-to-region beg end))
          (switch-to-buffer buf)))
    (widen)))

;;;###autoload
(defun doom/set-read-only-region (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p)))
    (add-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun doom/set-region-writeable (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))


;; Buffer Life and Death ;;;;;;;;;;;;;;;

(unless window-system
  (defalias 'wg-workgroup-associated-buffers 'ignore)
  (defalias 'wg-current-workgroup 'ignore)
  (defalias 'wg-save-session 'ignore))

;;;###autoload
(defun doom/get-buffers (&optional all-p)
  "Get all buffers in the current workgroup.

    If PROJECT-P is non-nil, get all buffers in current workgroup
    If both are non-nil, get all project buffers across all workgroups"
  (let ((buffers (if (wg-current-workgroup t)
                     (doom/get-buffers-in-workgroup)
                   (buffer-list)))
        project-root)
    (append (aif (and (not all-p) (doom/project-root t))
                (funcall (if (eq all-p 'not) '-remove '-filter)
                         (lambda (b) (projectile-project-buffer-p b it))
                         buffers)
              buffers)
            (list doom-buffer))))

;;;###autoload
(defun doom/get-buffers-in-workgroup ()
  (let ((assoc-bufs (wg-workgroup-associated-buffers nil)))
    (--filter (memq it assoc-bufs) (buffer-list))))

;;;###autoload
(defun doom/get-buffer-names (&optional buffer-list)
  (mapcar #'buffer-name (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-visible-windows (&optional window-list)
  "Get a list of the visible windows in the current frame (that aren't popups)."
  (-remove #'doom/popup-p (or window-list (window-list))))

;;;###autoload
(defun doom/get-visible-buffers (&optional buffer-list)
  "Get a list of unburied buffers in the current project and workgroup."
  (-filter #'get-buffer-window (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-buried-buffers (&optional buffer-list)
  "Get a list of buried buffers in the current project and workgroup."
  (-remove 'get-buffer-window (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-matching-buffers (pattern &optional buffer-list)
  "Get a list of buffers that match the pattern"
  (--filter (string-match-p pattern (buffer-name it))
            (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-buffers-in-modes (modes &optional buffer-list)
  "Get a list of buffers whose major-mode is one of MODES"
  (--filter (memq (buffer-local-value 'major-mode it) modes)
            (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/get-real-buffers (&optional buffer-list)
  (-filter #'doom/real-buffer-p (or buffer-list (doom/get-buffers))))

;;;###autoload
(defun doom/kill-real-buffer (&optional arg)
  "Kill buffer (but only bury scratch buffer), then switch to a real buffer. Only buries
the buffer if it is being displayed in another window."
  (interactive (list t))
  (cond ((eq (current-buffer) doom-buffer)
         (doom-mode-init t))
        (t
         (let* ((old-project (doom/project-root))
                (buffer (current-buffer))
                (only-buffer-window-p (= (length (get-buffer-window-list buffer nil t)) 1)))
           (when (and only-buffer-window-p buffer-file-name (buffer-modified-p))
             (if (yes-or-no-p "Buffer is unsaved, save it?")
                 (save-buffer)
               (set-buffer-modified-p nil)))
           (when arg
             (doom/previous-real-buffer)
             (when (eq buffer (current-buffer))
               (switch-to-buffer doom-buffer t t)
               (when (featurep 'core-scratch)
                 (doom|update-scratch-buffer old-project)))
             (when only-buffer-window-p
               (kill-buffer buffer))))))
  t)

;;;###autoload
(defun doom/kill-unreal-buffers ()
  "Kill all buried, unreal buffers in current frame. See `doom-unreal-buffers'"
  (interactive)
  (let ((kill-list (-remove 'doom/real-buffer-p
                            (doom/get-buried-buffers (buffer-list)))))
    (mapc 'kill-buffer kill-list)
    (doom/kill-process-buffers)
    (message "Cleaned up %s buffers" (length kill-list))))

;;;###autoload
(defun doom/kill-process-buffers ()
  "Kill all buffers that represent running processes and aren't visible."
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
(defun doom/kill-matching-buffers (regexp &optional buffer-list)
  (interactive)
  (let ((i 0))
    (mapc (lambda (b)
            (when (string-match-p regexp (buffer-name b))
              (kill-buffer b)
              (setq i (1+ i))))
          (if buffer-list buffer-list (doom/get-buffers)))
    (message "Killed %s matches" i)))

;;;###autoload
(defun doom/cycle-real-buffers (&optional n)
  "Switch to the previous buffer and avoid special buffers. If there's nothing
left, create a scratch buffer."
  (let* ((start-buffer (current-buffer))
         (move-func (if (> n 0) 'switch-to-next-buffer 'switch-to-prev-buffer))
         (max 25)
         (i 0)
         (continue t)
         (buffers (doom/get-real-buffers (doom/get-buffers t)))
         destbuf)
    (setq destbuf
          (catch 'goto
            (if (or (not buffers)
                    (= (length buffers) 1))
                (progn (message "No other buffers in workgroup")
                       (throw 'goto (current-buffer)))
              (funcall move-func)
              (while (not (memq (current-buffer) buffers))
                (if (or (eq (current-buffer) start-buffer)
                        (>= i max))
                    (throw 'goto doom-buffer)
                  (funcall move-func))
                (cl-incf i))
              (current-buffer))))
    (when (eq destbuf doom-buffer)
      (doom|update-scratch-buffer)
      (message "Nowhere to go"))
    (switch-to-buffer destbuf)))

;;;###autoload
(defun doom/real-buffer-p (&optional buffer)
  "Returns whether BUFFER a 'real' buffer or not. Real means it isn't a popup,
temporary, scratch or special buffer."
  (setq buffer (or (and (bufferp buffer) buffer)
                   (and (stringp buffer) (get-buffer buffer))
                   (current-buffer)))
  (or (eq buffer doom-buffer)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (not (or (apply #'derived-mode-p (-filter 'symbolp doom-unreal-buffers))
                   (--any? (string-match-p it (buffer-name buffer))
                           (-filter 'stringp doom-unreal-buffers))))))))

;; Inspired by spacemacs <https://github.com/syl20bnr/spacemacs/blob/master/spacemacs/funcs.el>
;;;###autoload
(defun doom/next-real-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (doom/cycle-real-buffers +1))

;;;###autoload
(defun doom/previous-real-buffer ()
  "Switch to the previous buffer and avoid special buffers."
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
  (doom--kill-buffers (--filter (not (eq it doom-buffer)) (doom/get-buffers (not bang))))
  (mapc (lambda (w) (when (eq (window-buffer w) doom-buffer)
                 (delete-window w)))
        (doom/get-visible-windows)))

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
  "Kill buried project buffers (in workgroup) and report how many it found. BANG = get all
buffers regardless of project."
  (interactive "<!>")
  (doom--kill-buffers (doom/get-buried-buffers (doom/get-buffers (not bang)))))

;;;###autoload (autoload 'doom:kill-buried-buffers "defuns-buffers" nil t)
(evil-define-command doom:kill-matching-buffers (&optional bang pattern)
  "Kill project buffers matching regex pattern PATTERN. If BANG, then extend search to
buffers regardless of project."
  :repeat nil
  (interactive "<!><a>")
  (doom-kill-buffers (doom/get-matching-buffers pattern (doom/get-buffers (not bang)))))

;;;###autoload (autoload 'doom:scratch-buffer "defuns-buffers" nil t)
(evil-define-operator doom:scratch-buffer (&optional beg end bang)
  "Send a selection to the scratch buffer. If BANG, don't use a popup."
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
      (setq mode-line-format (doom-modeline 'scratch))
      (when (and (not (eq major-mode mode))
                 (functionp mode))
        (funcall mode))
      (if text (insert text)))
    (if bang (switch-to-buffer new-buf) (doom/popup-buffer new-buf))))

;;;###autoload (autoload 'doom:cd "defuns-buffers" nil t)
(evil-define-command doom:cd (dir)
  "Ex-command alias for `cd'"
  :repeat nil
  (interactive "<f>")
  (cd (if (zerop (length dir)) "~" dir)))

;;;###autoload
(defun doom/kill-all-buffers-do-not-remember ()
  "Kill all buffers so that workgroups2 will wipe its current session."
  (interactive)
  (let (confirm-kill-emacs)
    (mapc 'kill-buffer (doom/get-buffers t))
    (kill-this-buffer)
    (delete-other-windows)
    (wg-save-session t)
    (save-buffers-kill-terminal)))

(provide 'defuns-buffers)
;;; defuns-buffers.el ends here
