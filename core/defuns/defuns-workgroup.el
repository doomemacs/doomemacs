;;; defuns-workgroup.el

;;;###autoload
(defun doom|wg-cleanup ()
  (doom/popup-close-all)
  (when (and (featurep 'neotree) (neo-global--window-exists-p))
    (neotree-hide)))

;;;###autoload
(defun doom/wg-projectile-switch-project ()
  (let ((project-root (doom/project-root)))
    (doom:workgroup-new nil (file-name-nondirectory (directory-file-name project-root)) t)
    (doom-reload-scratch-buffer project-root)
    (when (featurep 'neotree)
      (neotree-projectile-action))))

;;;###autoload (autoload 'doom:workgroup-save "defuns-workgroup" nil t)
(evil-define-command doom:workgroup-save (&optional bang session-name)
  (interactive "<!><a>")
  (unless (wg-workgroup-list)
    (wg-create-workgroup wg-first-wg-name))
  (doom|wg-cleanup)
  (wg-save-session-as (if session-name
                          (concat wg-workgroup-directory session-name)
                        (if bang
                            (concat wg-workgroup-directory (f-filename (doom/project-root)))
                          wg-session-file))))

;;;###autoload (autoload 'doom:workgroup-load "defuns-workgroup" nil t)
(evil-define-command doom:workgroup-load (&optional bang session-name)
  (interactive "<!><a>")
  (let ((session-file (if session-name
                          (concat wg-workgroup-directory session-name)
                        (let ((sess (concat wg-workgroup-directory (f-filename (doom/project-root)))))
                          (if bang
                              (when (file-exists-p sess)
                                sess)
                            wg-session-file)))))
    (unless session-file
      (user-error "No session found"))
    (wg-open-session session-file))
  (doom/workgroup-display t))

;;;###autoload
(defun doom/clear-sessions ()
  "Delete all session files."
  (interactive)
  (mapc 'delete-file (f-glob (expand-file-name "*" wg-workgroup-directory))))

;;;###autoload (autoload 'doom:workgroup-new "defuns-workgroup" nil t)
(evil-define-command doom:workgroup-new (bang name &optional silent)
  "Create a new workgroup. If BANG, overwrite any workgroup named NAME."
  (interactive "<!><a>")
  (unless name
    (setq name (format "#%s" (1+ (length (wg-session-workgroup-list (wg-current-session t)))))))
  (let ((new-wg (wg-get-workgroup name t)))
    (when (and new-wg bang)
      (wg-delete-workgroup new-wg)
      (setq new-wg nil))
    (setq new-wg (or new-wg (wg-make-and-add-workgroup name t)))
    (add-to-list 'doom-wg-names (wg-workgroup-uid new-wg))
    (wg-switch-to-workgroup new-wg))
  (unless silent
    (doom--workgroup-display (wg-previous-workgroup t)
                             (format "Created %s" name)
                             'success)))

;;;###autoload (autoload 'doom:workgroup-rename "defuns-workgroup" nil t)
(evil-define-command doom:workgroup-rename (bang &optional new-name)
  (interactive "<!><a>")
  (let* ((wg (wg-current-workgroup))
         (wg-uid (wg-workgroup-uid wg))
         (old-name (wg-workgroup-name wg)))
    (if bang
        (setq doom-wg-names (delete wg-uid doom-wg-names))
      (unless new-name
        (user-error "You didn't enter in a name"))
      (wg-rename-workgroup new-name wg)
      (add-to-list 'doom-wg-names wg-uid)
      (doom--workgroup-display wg (format "Renamed '%s'->'%s'" old-name new-name) 'success))))

;;;###autoload (autoload 'doom:workgroup-delete "defuns-workgroup" nil t)
(evil-define-command doom:workgroup-delete (&optional bang name)
  (interactive "<!><a>")
  (let* ((current-wg (wg-current-workgroup))
         (wg-name (or name (wg-workgroup-name current-wg))))
    (when bang
      (setq wg-name (wg-read-workgroup-name)))
    (let ((wg (wg-get-workgroup name)))
      (setq doom-wg-names (delete (wg-workgroup-uid wg) doom-wg-names))
      (if (eq wg current-wg)
          (wg-kill-workgroup)
        (wg-delete-workgroup wg))
      (doom--workgroup-display nil (format "Deleted %s" wg-name) 'success))))

;;;###autoload
(defun doom:kill-other-workgroups ()
  "Kill all other workgroups."
  (interactive)
  (let (workgroup (wg-current-workgroup))
    (dolist (w (wg-session-workgroup-list (wg-current-session t)))
      (unless (wg-current-workgroup-p w)
        (wg-kill-workgroup w)))))

(defun doom--workgroup-display (&optional suppress-update message message-face)
  (message "%s%s" (doom/workgroup-display suppress-update t)
           (propertize message 'face message-face)))

;;;###autoload
(defun doom/workgroup-display (&optional suppress-update return-p message)
  (interactive)
  (awhen (wg-current-session t)
    (unless (eq suppress-update t)
      (doom/workgroup-update-names (if (wg-workgroup-p suppress-update) suppress-update)))
    (let ((output (wg-display-internal
                   (lambda (workgroup index)
                     (if (not workgroup) wg-nowg-string
                       (wg-element-display
                        workgroup
                        (format " [%d] %s " (1+ index) (wg-workgroup-name workgroup))
                        'wg-current-workgroup-p)))
                   (wg-session-workgroup-list it))))
      (if return-p
          output
        (message "%s%s" output (or message ""))))))

;;;###autoload
(defun doom/workgroup-update-names (&optional wg)
  (let ((wg (or wg (wg-current-workgroup))))
    (unless (member (wg-workgroup-uid wg) doom-wg-names)
      (ignore-errors
        (let ((old-name (wg-workgroup-name wg))
              (new-name (f-filename (doom/project-root))))
          (unless (string= new-name old-name)
            (wg-rename-workgroup new-name wg)))))))

(defun doom--switch-to-workgroup (direction &optional count)
  (interactive "<c>")
  (assert (memq direction '(left right)))
  (condition-case err
      (progn
        (if count
            (wg-switch-to-workgroup-at-index (1- count))
          (funcall (intern (format "wg-switch-to-workgroup-%s" direction))))
        (doom/workgroup-display t))
      (error (doom/workgroup-display t nil (format "Nope! %s" (cadr err))))))

;;;###autoload (autoload 'doom:switch-to-workgroup-left "defuns-workgroup" nil t)
(evil-define-command doom:switch-to-workgroup-left (count)
  (interactive "<c>")
  (doom--switch-to-workgroup 'left))

;;;###autoload (autoload 'doom:switch-to-workgroup-right "defuns-workgroup" nil t)
(evil-define-command doom:switch-to-workgroup-right (count)
  (interactive "<c>")
  (doom--switch-to-workgroup 'right))

;;;###autoload
(defun doom:switch-to-workgroup-at-index (index)
  (interactive)
  (doom/workgroup-update-names)
  (let ((wg (nth index (wg-workgroup-list-or-error)))
        msg)
    (if wg
        (unless (eq wg (wg-current-workgroup t))
          (wg-switch-to-workgroup-at-index index))
      (setq msg (format "No tab #%s" (1+ index))))
    (doom/workgroup-display t nil msg)))

;;;###autoload
(defun doom/undo-window-change ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-undo-wconfig-change 'winner-undo)))

;;;###autoload
(defun doom/redo-window-change ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-redo-wconfig-change 'winner-redo)))

;;;###autoload
(defun doom/close-window-or-workgroup ()
  (interactive)
  (if (doom/popup-p)
      (doom/popup-close)
    (when (doom/kill-real-buffer)
      (if (and (one-window-p t)
               (> (length (wg-workgroup-list)) 1))
          (if (string= (wg-workgroup-name (wg-current-workgroup)) wg-first-wg-name)
              (evil-window-delete)
            (doom:workgroup-delete))
        (evil-window-delete)))))

(provide 'defuns-workgroup)
;;; defuns-workgroup.el ends here
