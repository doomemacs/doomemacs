;;; feature/workspaces/autoload.el

;;;###autoload
(defun +workspace-cleanup ()
  "Remove unsavable windows and buffers before we save the window
configuration."
  (let (doom-buffer-inhibit-refresh)
    (doom/popup-close-all)
    (when (and (featurep 'neotree) (neo-global--window-exists-p))
      (neotree-hide))))

;;;###autoload
(defun +workspace-save (session-file)
  "Save the current workspace as SESSION-FILE. Ensure a workgroup exists to be
saved."
  (+workspace-cleanup)
  (unless (wg-workgroup-list)
    (wg-create-workgroup wg-first-wg-name))
  (wg-save-session-as session-file))

;;;###autoload
(defun +workspace-load (session-file)
  "Load a workspace from SESSION-FILE."
  (+workspace-cleanup)
  (when (not (and session-file (file-exists-p session-file)))
    (user-error "No session found"))
  (wg-open-session session-file))

;;;###autoload
(defun +workspace-new (&optional session-name overwrite-p)
  "Create a new workspace, named SESSION-NAME (optional) and return it (without
switching to it). Otherwise, it's named #N, where N is the number of current
workgroups + 1."
  (unless session-name
    (setq session-name (format "#%s" (1+ (length (wg-session-workgroup-list (wg-current-session t)))))))
  (awhen (and overwrite-p (wg-get-workgroup session-name t))
    (wg-delete-workgroup new-wg))
  (let ((new-wg (wg-make-and-add-workgroup session-name t)))
    (add-to-list '+workspace-names (wg-workgroup-uid new-wg))
    new-wg))

;;;###autoload
(defun +workspace-rename (new-name)
  "Rename the current workspace to NEW-NAME."
  (unless new-name
    (user-error "You didn't enter in a name"))
  (let ((wg (wg-current-workgroup)))
    (wg-rename-workgroup new-name wg)
    (add-to-list '+workspace-names (wg-workgroup-uid wg))
    wg))

;;;###autoload
(defun +workspace-delete (&optional session-name)
  "Delete the current workspace."
  (let* ((current-wg (wg-current-workgroup t))
         (wg (if session-name (wg-get-workgroup session-name t) current-wg)))
    (unless wg
      (user-error "No workgroup found with that name: %s" session-name))
    (setq +workspace-names (delete (wg-workgroup-uid wg) +workspace-names))
    (if (eq wg current-wg)
        (wg-kill-workgroup)
      (wg-delete-workgroup wg))))

;;;###autoload (autoload '+workspace:save "feature/workspaces/autoload" nil t)
;;;###autoload (autoload '+workspace:load "feature/workspaces/autoload" nil t)
;;;###autoload (autoload '+workspace:new "feature/workspaces/autoload" nil t)
;;;###autoload (autoload '+workspace:rename "feature/workspaces/autoload" nil t)
;;;###autoload (autoload '+workspace:delete "feature/workspaces/autoload" nil t)
;;;###autoload (autoload '+workspace:jump-to "feature/workspaces/autoload" nil t)
;;;###autoload (autoload '+workspace:switch-left "feature/workspaces/autoload" nil t)
;;;###autoload (autoload '+workspace:switch-right "feature/workspaces/autoload" nil t)

(after! evil
  (evil-define-command +workspace:save (&optional bang name)
    (interactive "<!><a>")
    (+workspace-save
     (cond (name (concat wg-workgroup-directory session-name))
           (bang (concat wg-workgroup-directory (f-filename (doom-project-root))))
           (t wg-session-file))))

  (evil-define-command +workspace:load (&optional bang name)
    (interactive "<!><a>")
    (+workspace-load
     (cond (name (concat wg-workgroup-directory session-name))
           (bang
            (concat wg-workgroup-directory (f-filename (doom-project-root))))
           (t wg-session-file)))
    (+workspace/display t))

  (evil-define-command +workspace:new (bang name)
    "Create a new workgroup named NAME. If BANG, overwrite any workgroup named
NAME. If NAME is omitted, autogenerate a name."
    (interactive "<!><a>")
    (let* ((wg (+workspace-new name bang))
           (wg-name (wg-workgroup-name wg)))
      (wg-switch-to-workgroup wg)
      (workspace--display-inline (wg-previous-workgroup t)
                                      (format "Created %s" wg-name)
                                      'success)))

  (evil-define-command +workspace:rename (&optional bang new-name)
    "Rename the current workgroup to NEW-NAME. If BANG and this workgroup has a
fixed name, un-fix it."
    (interactive "<!><a>")
    (let* ((wg (wg-current-workgroup))
           (wg-uid (wg-workgroup-uid wg))
           (old-name (wg-workgroup-name wg)))
      (if bang
          (progn
            (setq +workspace-names (delete wg-uid +workspace-names))
            (workspace--display-inline t (format "Unfixed '%s'" old-name) 'success))
        (+workspace-rename new-name)
        (workspace--display-inline nil (format "Renamed '%s'->'%s'" old-name new-name) 'success))))

  (evil-define-command +workspace:delete (&optional bang name)
    "Delete the workgroup specified by NAME. If NAME is omitted, delete the
current workgroup. If BANG, prompts the user for which workgroup to delete."
    (interactive "<!><a>")
    (when bang
      (setq name (wg-read-workgroup-name))
      (unless name
        (user-error "You didn't select a workgroup.")))
    (let ((wg-name (ignore-errors (wg-workgroup-name (wg-current-workgroup t)))))
      (+workspace-delete name)
      (workspace--display-inline nil (format "Deleted %s" wg-name) 'success)))

  (evil-define-command +workspace:jump-to (&optional search)
    (interactive "<a>")
    (if search
        (progn) ;; TODO Fuzzy matching
      (awhen (wg-read-workgroup-name)
        (wg-switch-to-workgroup it))))

  (evil-define-command +workspace:switch-left (&optional count)
    "Switch to the previous workspace on the right. If COUNT, switch to the workspace
at that index counting from the end."
    (interactive "<c>")
    (if count
        (+workspace/activate-at (- 0 count))
      (+workspace--switch-in-direction 'left)))

  (evil-define-command +workspace:switch-right (&optional count)
    "Switch to the next workspace on the right. If COUNT, switch to the workspace
at that index."
    (interactive "<c>")
    (if count
        (+workspace/activate-at (max 0 (1- count)))
      (+workspace--switch-in-direction 'right))))

;;;###autoload
(defun +workspace/kill-others ()
  "Kill all other workspaces, besides the current one."
  (interactive)
  (let (workgroup (wg-current-workgroup))
    (dolist (w (wg-session-workgroup-list (wg-current-session t)))
      (unless (wg-current-workgroup-p w)
        (wg-kill-workgroup w)))))

(defun workspace--display-inline (&optional suppress-update message message-face)
  (message "%s%s" (+workspace/display suppress-update t)
           (propertize message 'face message-face)))

;;;###autoload
(defun +workspace/display (&optional suppress-update return-p message)
  "Display all the open workspaces in the minibuffer, like tabs."
  (interactive)
  (awhen (wg-current-session t)
    (unless (eq suppress-update t)
      (+workgroup--update-names (if (wg-workgroup-p suppress-update) suppress-update)))
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

(defun +workgroup--update-names (&optional wg)
  (let ((wg (or wg (wg-current-workgroup))))
    (unless (member (wg-workgroup-uid wg) +workspace-names)
      (ignore-errors
        (let ((old-name (wg-workgroup-name wg))
              (new-name (f-filename (doom-project-root))))
          (unless (string= new-name old-name)
            (wg-rename-workgroup new-name wg)))))))

(defun +workspace--switch-in-direction (direction &optional count)
  (interactive "<c>")
  (assert (memq direction '(left right)))
  (condition-case err
      (progn
        (if count
            (wg-switch-to-workgroup-at-index (1- count))
          (funcall (intern (format "wg-switch-to-workgroup-%s" direction))))
        (+workspace/display t))
      (error (+workspace/display t nil (format "Nope! %s" (cadr err))))))

;;;###autoload
(defun +workspace-switch-last ()
  "Switch to the last workspace."
  (interactive)
  (let ((count (length (wg-session-workgroup-list (wg-current-session t)))))
    (+workspace/activate-at (max 0 (1- count)))))

;;;###autoload
(defun +workspace/activate-at (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive)
  (+workgroup--update-names)
  (let ((wg-list (wg-workgroup-list-or-error)))
    (when (< index 0)
      (setq index (- (length wg-list) (abs index))))
    (let ((wg (nth index wg-list))
          msg)
      (if wg
          (unless (eq wg (wg-current-workgroup t))
            (wg-switch-to-workgroup-at-index index))
        (setq msg (format "No tab #%s" (1+ index))))
      (+workspace/display t nil msg))))

;;;###autoload
(defun +workspace/undo ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-undo-wconfig-change 'winner-undo)))

;;;###autoload
(defun +workspace/redo ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-redo-wconfig-change 'winner-redo)))

;;;###autoload
(defun +workspace/new-frame ()
  "Create a new frame, and create a new, blank workgroup within it. Also ensure
nlinum behaves in the process."
  (interactive)
  (let ((nlinum-p (and (featurep 'nlinum)
                       (memq 'nlinum--setup-window window-configuration-change-hook))))
    ;; Disable nlinum to fix elusive "invalid face linum" bug
    (remove-hook 'window-configuration-change-hook 'nlinum--setup-window t)
    (let ((frame (new-frame))
          (frame-name (format "*new-%s*" (length +workspace-frames))))
      (with-selected-frame frame
        (wg-create-workgroup frame-name t)
        (add-to-list '+workspace-frames (cons frame frame-name))))
    (when nlinum-p
      (add-hook 'window-configuration-change-hook 'nlinum--setup-window nil t))))

;;;###autoload
(defun +workspace/close-frame ()
  (interactive)
  (let ((frame (assq (selected-frame) +workspace-frames)))
    (if frame
        (progn (wg-delete-workgroup (wg-get-workgroup (cdr frame)))
               (delete-frame (car frame)))
      (delete-frame))))

;;;###autoload
(defun +workspace/or-window-close ()
  (interactive)
  (if (doom-popup-p)
      (doom/popup-close)
    (when (doom/kill-real-buffer)
      (if (and (one-window-p t)
               (> (length (wg-workgroup-list)) 1))
          (if (string= (wg-workgroup-name (wg-current-workgroup)) wg-first-wg-name)
              (evil-window-delete)
            (+workspace:delete))
        (evil-window-delete)))))
