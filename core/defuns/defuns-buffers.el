;;; defuns-buffers.el

;;;###autoload (autoload 'narf:narrow "defuns-buffers" nil t)
(evil-define-operator narf:narrow (&optional beg end bang)
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
(defun narf/set-read-only-region (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p)))
    (add-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun narf/set-region-writeable (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))


;; Buffer Life and Death ;;;;;;;;;;;;;;;

;;;###autoload
(defun narf/get-buffers (&optional project-p)
  "Get all buffers in the current workgroup.

    If PROJECT-P is non-nil, get all buffers in current workgroup
    If both are non-nil, get all project buffers across all workgroups"
  (let ((buffers (buffer-list))
        project-root)
    (if (and project-p (setq project-root (narf/project-root t)))
        (funcall (if (eq project-p 'not) '-remove '-filter)
                 (lambda (b) (projectile-project-buffer-p b project-root))
                 buffers)
      buffers)))

;;;###autoload
(defun narf/get-visible-windows (&optional buffer-list)
  (-map #'get-buffer-window
        (narf/get-visible-buffers (or buffer-list (narf/get-buffers)))))

;;;###autoload
(defun narf/get-visible-buffers (&optional buffer-list)
  "Get a list of buffers that are not buried (i.e. visible)"
  (-filter #'get-buffer-window (or buffer-list (narf/get-buffers))))

;;;###autoload
(defun narf/get-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried (i.e. not visible)"
  (let* ((buffers (or buffer-list (narf/get-buffers)))
         (old-len (length buffers)))
    (-remove 'get-buffer-window buffers)))

;;;###autoload
(defun narf/get-matching-buffers (pattern &optional buffer-list)
  "Get a list of buffers that match the pattern"
  (--filter (string-match-p pattern (buffer-name it))
            (or buffer-list (narf/get-buffers))))

;;;###autoload
(defun narf/get-buffers-in-modes (modes &optional buffer-list)
  "Get a list of buffers whose major-mode is one of MODES"
  (--filter (with-current-buffer it (memq major-mode modes))
            (or buffer-list (narf/get-buffers))))

;;;###autoload
(defun narf/get-real-buffers (&optional buffer-list)
  (-filter #'narf/real-buffer-p (or buffer-list (narf/get-buffers))))

;;;###autoload
(defun narf/kill-real-buffer ()
  "Kill buffer (but only bury scratch buffer), then switch to a real buffer. Only buries
the buffer if it is being displayed in another window."
  (interactive)
  (cond ((string-match-p "^\\*scratch\\*" (buffer-name))
         (erase-buffer))
        (t
         (if (> (length (get-buffer-window-list (current-buffer) nil nil)) 1)
             (bury-buffer)
           (kill-this-buffer))))
  (if (narf/popup-p (selected-window))
      (narf/popup-close)
    (unless (narf/real-buffer-p (current-buffer))
      (narf/previous-real-buffer))))

;;;###autoload
(defun narf/kill-unreal-buffers ()
  "Kill all buried, unreal buffers in current frame. See `narf-unreal-buffers'"
  (interactive)
  (let* ((all-buffers (narf/get-buffers))
         (real-buffers (narf/get-real-buffers all-buffers))
         (kill-list (--filter (not (memq it real-buffers))
                              (narf/get-buried-buffers all-buffers))))
    (mapc 'kill-buffer kill-list)
    (narf/kill-process-buffers)
    (message "Cleaned up %s buffers" (length kill-list))))

;;;###autoload
(defun narf/kill-process-buffers ()
  "Kill all buffers that represent running processes and aren't visible."
  (interactive)
  (let ((buffer-list (narf/get-buffers))
        (killed-processes 0))
    (dolist (p (process-list))
      (let* ((process-name (process-name p))
             (assoc (assoc process-name narf-cleanup-processes-alist)))
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
(defun narf/kill-matching-buffers (regexp &optional buffer-list)
  (interactive)
  (let ((i 0))
    (mapc (lambda (b)
            (when (string-match-p regexp (buffer-name b))
              (kill-buffer b)
              (setq i (1+ i))))
          (if buffer-list buffer-list (narf/get-buffers)))
    (message "Killed %s matches" i)))

;;;###autoload
(defun narf/cycle-real-buffers (&optional n scratch-default)
  "Switch to the previous buffer and avoid special buffers. If there's nothing
left, create a scratch buffer."
  (let* ((start-buffer (current-buffer))
         (move-func (if (< n 0) 'switch-to-next-buffer 'switch-to-prev-buffer))
         (real-buffers (narf/get-real-buffers))
         (realc (length real-buffers))
         (max 25)
         (i 0)
         (continue t))
    (if (or (= realc 0)
            (and (= realc 1) (eq (car real-buffers) (current-buffer))))
        (progn
          (narf|update-scratch-buffer-cwd)
          (switch-to-buffer "*scratch*")
          (message "Nowhere to go"))
      (funcall move-func)
      (while (and continue (< i max))
        (let ((current-buffer (current-buffer)))
          (cond ((eq current-buffer start-buffer)
                 (when scratch-default
                   (narf|update-scratch-buffer-cwd)
                   (switch-to-buffer "*scratch*"))
                 (setq continue nil))
                ((not (memq current-buffer real-buffers))
                 (funcall move-func))
                (t
                 (setq continue nil))))
        (cl-incf i)))))

;;;###autoload
(defun narf/real-buffer-p (&optional buffer-or-name)
  (let ((buffer (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))
    (when (buffer-live-p buffer)
      (not (--any? (if (stringp it)
                       (string-match-p it (buffer-name buffer))
                     (eq (buffer-local-value 'major-mode buffer) it))
                   narf-unreal-buffers)))))

;; Inspired by spacemacs <https://github.com/syl20bnr/spacemacs/blob/master/spacemacs/funcs.el>
;;;###autoload
(defun narf/next-real-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (narf/cycle-real-buffers +1))

;;;###autoload
(defun narf/previous-real-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (narf/cycle-real-buffers -1))

(defun narf--kill-buffers (buffers &optional filter-func)
  (let ((buffers (if filter-func (funcall filter-func buffers) buffers))
        (affected 0))
    (mapc (lambda (b) (when (kill-buffer b) (incf affected))) buffers)
    (unless (narf/real-buffer-p)
      (narf/previous-real-buffer))
    (message "Killed %s buffers" affected)))

;;;###autoload (autoload 'narf:kill-all-buffers "defuns-buffers" nil t)
(evil-define-command narf:kill-all-buffers (&optional bang)
  "Kill all project buffers. If BANG, kill *all* buffers (in workgroup)."
  (interactive "<!>")
  (narf--kill-buffers (narf/get-buffers (not bang)))
  (when bang
    (delete-other-windows)))

;;;###autoload (autoload 'narf:kill-buried-buffers "defuns-buffers" nil t)
(evil-define-command narf:kill-buried-buffers (&optional bang)
  "Kill buried project buffers (in workgroup) and report how many it found. BANG = get all
buffers regardless of project."
  (interactive "<!>")
  (narf-kill-buffers (narf/get-buffers (not bang)) 'narf/get-buried-buffers))

;;;###autoload (autoload 'narf:kill-buried-buffers "defuns-buffers" nil t)
(evil-define-command narf:kill-matching-buffers (&optional bang pattern)
  "Kill project buffers matching regex pattern PATTERN. If BANG, then extend search to
buffers regardless of project."
  :repeat nil
  (interactive "<!><a>")
  (narf-kill-buffers (narf/get-matching-buffers pattern (narf/get-buffers (not bang)))))

;;;###autoload (autoload 'narf:send-to-scratch-or-org "defuns-buffers" nil t)
(evil-define-operator narf:send-to-scratch-or-org (&optional beg end bang)
  "Send a selection to the scratch buffer. If BANG, then send it to org-capture instead."
  :move-point nil
  :type inclusive
  (interactive "<r><!>")
  (let ((mode major-mode)
        (text (when (and (evil-visual-state-p) beg end)
                (buffer-substring beg end))))
    (if bang
        (org-capture-string text)
      ;; or scratch buffer by default
      (let* ((project-dir (narf/project-root t))
             (buffer-name "*scratch*"))
        (narf/popup-buffer buffer-name)
        (with-current-buffer buffer-name
          (when project-dir
            (cd project-dir))
          (if text (insert text))
          (funcall mode))
        ))))

;;;###autoload (autoload 'narf:cd "defuns-buffers" nil t)
(evil-define-command narf:cd (dir)
  "Ex-command alias for `cd'"
  :repeat nil
  (interactive "<f>")
  (cd (if (zerop (length dir)) "~" dir)))

;;;###autoload
(defun narf/kill-all-buffers-do-not-remember ()
  "Kill all buffers so that workgroups2 will wipe its current session."
  (interactive)
  (let ((confirm-kill-emacs nil))
    (mapc 'kill-buffer (narf/get-buffers))
    (kill-this-buffer)
    (delete-other-windows)
    (wg-save-session t)
    (save-buffers-kill-terminal)))

(provide 'defuns-buffers)
;;; defuns-buffers.el ends here
