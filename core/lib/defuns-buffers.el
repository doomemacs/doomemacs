;;; defuns-buffers.el

;;;###autoload
(defun narf:narrow (start end)
  "Restrict editing in this buffer to the current region, indirectly.

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

;;;###autoload
(defun narf:widen ()
  (interactive)
  (when (buffer-narrowed-p)
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


;; Killing Life and Death ;;;;;;;;;;;;;;

;;;###autoload
(defun narf:kill-real-buffer ()
  "Kill buffer (but only bury scratch buffer), then switch to a living buffer."
  (interactive)
  (let ((bname (buffer-name)))
    (cond ((string-match-p "^\\*scratch\\*" bname)
           (erase-buffer)
           (bury-buffer))
          ((string-equal "*" (substring bname 0 1)))
          (t (kill-this-buffer))))
  (unless (narf/real-buffer-p (current-buffer))
    (narf/previous-real-buffer)))

;;;###autoload
(defun narf/get-visible-buffers (&optional buffer-list)
  "Get a list of buffers that are not buried (i.e. visible)"
  (-remove #'get-buffer-window (or buffer-list (buffer-list))))

;;;###autoload
(defun narf/get-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried (i.e. not visible)"
  (-filter #'get-buffer-window (or buffer-list (buffer-list))))

;;;###autoload
(defun narf/get-matching-buffers (pattern &optional buffer-list)
  "Get a list of buffers that match the pattern"
  (--filter (string-match-p pattern (buffer-name it)) (or buffer-list (buffer-list))))

;;;###autoload
(defun narf/get-real-buffers(&optional buffer-list)
  (-filter (lambda (buffer)
             (not (--any? (if (stringp it)
                              (string-match-p it (buffer-name buffer))
                            (eq (buffer-local-value 'major-mode buffer) it))
                          narf-unreal-buffers)))
            (or buffer-list (buffer-list))))

;;;###autoload
(defun narf:kill-unreal-buffers ()
  "Kill all buried, unreal buffers in current frame. See `narf-unreal-buffers'"
  (interactive)
  (let* ((real-buffers (narf/get-real-buffers))
         (kill-list (--filter (not (memq it real-buffers)) narf/get-buried-buffers)))
    (message "Cleaned up %s buffers" (length kill-list))
    (mapc 'kill-buffer kill-list)
    (narf:kill-process-buffers)))

;;;###autoload
(defun narf:kill-process-buffers ()
  "Kill all buffers that represent running processes and aren't visible."
  (interactive)
  (let ((buffer-list (buffer-list)))
    (dolist (p (process-list))
      (let* ((process-name (process-name p))
             (assoc (assoc process-name narf-cleanup-processes-alist)))
        (when (and assoc
                   (not (string= process-name "server"))
                   (process-live-p p)
                   (not (--any? (let ((mode (buffer-local-value 'major-mode it)))
                                  (eq mode (cdr assoc)))
                                buffer-list)))
          (message "Cleanup: killing %s" process-name)
          (delete-process p))))))

;;;###autoload
(defun narf:kill-matching-buffers (regexp &optional buffer-list)
  (interactive)
  (mapc (lambda (b)
          (if (string-match-p regexp (buffer-name b))
              (kill-buffer b)))
        (if buffer-list buffer-list (buffer-list))))

;;;###autoload
(defun narf/cycle-real-buffers (&optional n)
  "Switch to the previous buffer and avoid special buffers. If there's nothing
left, create a scratch buffer."
  (let ((start-buffer (current-buffer))
        (move-func (if (< n 0) 'switch-to-next-buffer 'switch-to-prev-buffer))
        (real-buffers (narf/get-real-buffers)))
    (funcall move-func)
    (while (let ((current-buffer (current-buffer)))
             (and (if (eq current-buffer start-buffer)
                      (ignore (switch-to-buffer "*scratch*"))
                    t)
                  (not (= n 0))
                  (not (eq current-buffer start-buffer))
                  (not (memq current-buffer real-buffers))))
      (setq n (1- n))
      (funcall move-func))))

;;;###autoload
(defun narf/real-buffer-p (&optional buffer-or-name)
  (let ((buffer (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))
    (when (buffer-live-p buffer)
      (not (--any? (if (stringp it)
                       (string-match-p it (buffer-name buffer))
                     (eq (buffer-local-value 'major-mode buffer) it))
                   narf-unreal-buffers)))))

;; From spacemacs <https://github.com/syl20bnr/spacemacs/blob/master/spacemacs/funcs.el>
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

;;;###autoload (autoload 'narf:kill-buried-buffers "defuns-buffers" nil t)
(evil-define-command narf:kill-buried-buffers (&optional bang)
  :repeat nil
  (interactive "<!>")
  (narf:kill-buried-buffers)
  (mapc 'kill-buffer
        (narf/get-buried-buffers (if bang (projectile-project-buffers) (buffer-list)))))

;;;###autoload (autoload 'narf:kill-all-buffers "defuns-buffers" nil t)
(evil-define-command narf:kill-all-buffers (&optional bang)
  "Kill all project buffers. If BANG, kill *all* buffers."
  :repeat nil
  (interactive "<!>")
  (if (and (not bang) (projectile-project-p))
      (projectile-kill-buffers)
    (mapc 'kill-buffer (buffer-list)))
  (delete-other-windows)
  (unless (narf/real-buffer-p)
    (narf/previous-real-buffer)))

;;;###autoload (autoload 'narf:scratch-buffer "defuns-buffers" nil t)
(evil-define-operator narf:scratch-buffer (&optional beg end bang)
  "Send a selection to the scratch buffer. If BANG, then send it to org-capture
  instead."
  :move-point nil
  :type inclusive
  (interactive "<r><!>")
  (let ((mode major-mode)
        (text (when (and (evil-visual-state-p) beg end)
                (buffer-substring beg end))))
    (if bang
        ;; use org-capture with bang
        (if text
            (org-capture-string text)
          (org-capture))
      ;; or scratch buffer by default
      (let* ((project-dir (narf/project-root t))
             (buffer-name "*scratch*"))
        (popwin:popup-buffer (get-buffer-create buffer-name))
        (when (eq (get-buffer buffer-name) (current-buffer))
          (when project-dir
            (cd project-dir))
          (if text (insert text))
          (funcall mode))))))

;;;###autoload (autoload 'narf:cd "defuns-buffers" nil t)
(evil-define-command narf:cd (dir)
  :repeat nil
  (interactive "<f>")
  (cd (if (zerop (length dir)) "~" dir)))

;;;###autoload
(defun narf/kill-all-buffers-do-not-remember ()
  (interactive)
  (let ((confirm-kill-emacs nil))
    (mapc 'kill-buffer (buffer-list))
    (kill-this-buffer)
    (delete-other-windows)
    (wg-save-session t)
    (save-buffers-kill-terminal)))


(provide 'defuns-buffers)
;;; defuns-buffers.el ends here
