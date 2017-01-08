;;; defuns-eshell.el

(require 'eshell)

(defvar doom-eshell-buffers '() "")

;;;###autoload
(defun doom|eshell-cleanup ()
  (when (eq major-mode 'eshell-mode)
    (setq doom-eshell-buffers (delete (current-buffer) doom-eshell-buffers))
    (cond ((doom/popup-p)
           (delete-window))
          ((string= "eshell" (wg-workgroup-name (wg-current-workgroup t)))
           (if (one-window-p)
               (doom:workgroup-delete)
             (delete-window))))))

;;;###autoload
(defun doom|eshell-init ()
  (when (eq major-mode 'eshell-mode)
    (add-to-list 'doom-eshell-buffers (current-buffer))))

(defun doom--eshell-outside-prompt-p (&optional offset)
  (< (point) eshell-last-output-end))

(defun doom--eshell-current-git-branch ()
    (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

;;;###autoload
(defun doom/eshell-split ()
  (interactive)
  (select-window (split-window-vertically))
  (doom:eshell))

;;;###autoload
(defun doom/eshell-vsplit ()
  (interactive)
  (select-window (split-window-horizontally))
  (doom:eshell))

;;;###autoload
(defun doom/eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (doom--eshell-current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " λ " 'face 'font-lock-constant-face)))

;;;###autoload (autoload 'doom:eshell "defuns-eshell" nil t)
(evil-define-command doom:eshell (&optional bang)
  "Create a shell in the current buffer. If BANG, use a popup buffer."
  (interactive "<!>")
  (let ((buf (if bang
                 (get-buffer-create "*eshell:popup*")
               (generate-new-buffer eshell-buffer-name))))
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode) (eshell-mode)))
    (if bang
        (doom/popup-buffer buf)
      (pop-to-buffer-same-window buf))))

;;;###autoload
(defun doom/eshell-tab ()
  "Create a separate tab for the shell."
  (interactive)
  (unless (wg-switch-to-workgroup (wg-get-workgroup "eshell" t) t)
    (doom:tab-create nil "eshell"))
  (let ((buf (--find (string-match-p "^\\*eshell" (buffer-name (window-buffer it)))
                     (doom/get-visible-windows))))
    (if buf
        (select-window (get-buffer-window buf))
      (doom:eshell))
    (doom/workgroup-display)))

;;;###autoload
(defun doom/eshell-frame ()
  "Create a separate frame for the shell."
  (interactive)
  (doom/new-frame)
  (doom:eshell))

;;;###autoload
(defun doom/eshell-evil-append ()
  (interactive)
  (goto-char eshell-last-output-end)
  (call-interactively 'evil-append-line))

;;;###autoload
(defun doom/eshell-evil-append-maybe ()
  (interactive)
  (if (doom--eshell-outside-prompt-p)
      (doom/eshell-evil-append)
    (call-interactively 'evil-append)))

;;;###autoload
(defun doom/eshell-evil-prepend ()
  (interactive)
  (goto-char eshell-last-output-end)
  (call-interactively 'evil-insert))

;;;###autoload
(defun doom/eshell-evil-prepend-maybe ()
  (interactive)
  (if (doom--eshell-outside-prompt-p)
      (doom/eshell-evil-prepend)
    (call-interactively 'evil-insert)))

;;;###autoload
(defun doom/eshell-evil-replace-maybe ()
  (interactive)
  (if (doom--eshell-outside-prompt-p)
      (user-error "Cannot edit read-only region")
    (call-interactively 'evil-replace)))

;;;###autoload
(defun doom/eshell-evil-replace-state-maybe ()
  (interactive)
  (if (doom--eshell-outside-prompt-p)
      (user-error "Cannot edit read-only region")
    (call-interactively 'evil-replace-state)))

;;;###autoload
(defun doom/eshell-evil-change ()
  (interactive)
  (when (doom--eshell-outside-prompt-p)
    (goto-char eshell-last-output-end))
  (call-interactively 'evil-change))

;;;###autoload
(defun doom/eshell-evil-change-line ()
  (interactive)
  (when (doom--eshell-outside-prompt-p)
    (goto-char eshell-last-output-end))
  (call-interactively 'evil-change-line))

(provide 'defuns-eshell)
;;; defuns-eshell.el ends here
