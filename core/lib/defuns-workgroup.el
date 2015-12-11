;;; defuns-workgroup.el

;;;###autoload
(defun narf/wg-helm-switch-to-workgroup (name)
  (wg-switch-to-workgroup (wg-get-workgroup name)))

;;;###autoload
(defun narf:helm-wg ()
  (interactive)
  (require 'helm)
  (helm :sources '(narf/helm-source-wg)))

;;;###autoload
(defun narf/wg-projectile-switch-project ()
  (let ((workgroup-name (file-name-nondirectory (directory-file-name (narf/project-root)))))
    (wg-create-workgroup workgroup-name t)
    (helm-projectile-find-file)))

;;;###autoload (autoload 'narf:save-session "defuns-workgroup" nil t)
(evil-define-command narf:save-session (&optional bang session-name)
  (interactive "<!><a>")
  (if session-name
      (wg-save-session-as (concat wg-workgroup-directory session-name) (not bang))
    (wg-save-session)))

;;;###autoload (autoload 'narf:load-session "defuns-workgroup" nil t)
(evil-define-command narf:load-session (&optional bang session-name)
  (interactive "<!><a>")
  (wg-open-session (if session-name
                       (concat wg-workgroup-directory session-name)
                     wg-session-file))
  (narf/workgroup-display t))

;;;###autoload (autoload 'narf:workgroup-new "defuns-workgroup" nil t)
(evil-define-command narf:workgroup-new (bang name &optional silent)
  "Create a new workgroup. If BANG, clone the current one to it."
  (interactive "<!><a>")
  (unless name
    (setq name (format "#%s" (length (wg-workgroup-list)))))
  (if bang
      (wg-clone-workgroup (wg-current-workgroup) name)
    (wg-create-workgroup name t))
  (unless silent
    (narf/workgroup-display (wg-previous-workgroup))))

;;;###autoload (autoload 'narf:workgroup-rename "defuns-workgroup" nil t)
(evil-define-command narf:workgroup-rename (new-name)
  (interactive "<a>")
  (let ((wg (wg-current-workgroup)))
    (wg-rename-workgroup new-name wg)
    (add-to-list 'narf-wg-names wg)))

;;;###autoload (autoload 'narf:workgroup-delete "defuns-workgroup" nil t)
(evil-define-command narf:workgroup-delete (&optional bang name)
  (interactive "<!><a>")
  (let* ((current-wg (wg-current-workgroup))
         (wg-name (or name (wg-workgroup-name current-wg))))
    (when bang
      (setq wg-name (wg-read-workgroup-name)))
    (let ((wg (wg-get-workgroup name)))
      (if (eq wg current-wg)
          (wg-kill-workgroup)
        (wg-delete-workgroup wg))
      (message "%s [Deleted %s]" (narf/workgroup-display nil t) wg-name))))

;;;###autoload
(defun narf:kill-other-workgroups ()
  "Kill all other workgroups."
  (interactive)
  (let (workgroup (wg-current-workgroup))
    (dolist (w (wg-workgroup-list))
      (unless (wg-current-workgroup-p w)
        (wg-kill-workgroup w)))))

;;;###autoload
(defun narf/workgroup-display (&optional suppress-update return-p)
  (interactive)
  (when (wg-current-session t)
    (unless (eq suppress-update t)
      (narf/workgroup-update-names (if (wg-workgroup-p suppress-update) suppress-update)))
    (let ((output (wg-display-internal
                   (lambda (workgroup index)
                     (if (not workgroup) wg-nowg-string
                       (wg-element-display
                        workgroup
                        (format " (%d) %s " (1+ index) (wg-workgroup-name workgroup))
                        'wg-current-workgroup-p)))
                   (wg-workgroup-list))))
      (if return-p
          output
        (message "%s" output)))))

;;;###autoload
(defun narf/workgroup-update-names (&optional wg)
  (let ((wg (or wg (wg-current-workgroup))))
    (unless (memq wg narf-wg-names)
      (ignore-errors
        (let ((old-name (wg-workgroup-name wg))
              (base (f-filename (buffer-file-name))))
          (unless (string= base old-name)
            (wg-rename-workgroup base wg)))))))
;; (advice-add 'select-window :after 'narf|workgroup-update-name)

;;;###autoload (autoload 'narf:switch-to-workgroup-left "defuns-workgroup" nil t)
(evil-define-command narf:switch-to-workgroup-left (count)
  (interactive "<c>")
  (if count
      (wg-switch-to-workgroup-at-index (1- count))
    (wg-switch-to-workgroup-left))
  (narf/workgroup-display (wg-previous-workgroup)))

;;;###autoload (autoload 'narf:switch-to-workgroup-right "defuns-workgroup" nil t)
(evil-define-command narf:switch-to-workgroup-right (count)
  (interactive "<c>")
  (if count
      (wg-switch-to-workgroup-at-index (1- count))
    (wg-switch-to-workgroup-right))
  (narf/workgroup-display (wg-previous-workgroup)))

;;;###autoload
(defun narf:switch-to-workgroup-at-index (index)
  (interactive)
  (narf/workgroup-update-names)
  (let ((wgs (wg-workgroup-list-or-error)))
    (unless (eq (nth index wgs) (wg-current-workgroup t))
      (wg-switch-to-workgroup-at-index index)))
  (narf/workgroup-display t))

;;;###autoload
(defun narf/undo-window-change ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-undo-wconfig-change 'winner-undo)))

;;;###autoload
(defun narf/redo-window-change ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-redo-wconfig-change 'winner-redo)))

;;;###autoload
(defun narf/close-window-or-workgroup ()
  (interactive)
  (if (and (= (length (window-list)) 1)
           (> (length (wg-workgroup-list)) 1))
      (if (string= (wg-workgroup-name (wg-current-workgroup)) wg-first-wg-name)
          (evil-window-delete)
        (narf:workgroup-delete))
    (evil-window-delete)))


(provide 'defuns-workgroup)
;;; defuns-workgroup.el ends here
