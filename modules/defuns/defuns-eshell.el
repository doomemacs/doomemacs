;;; defuns-eshell.el

(require 'eshell)

(defvar doom-eshell-buffers '() "")
(defvar doom-eshell-height 16 "")
(defvar-local doom-eshell-direction nil "")

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
  (select-window (split-window-vertically doom-eshell-height))
  (setq-local doom-eshell-direction 'below)
  (doom--eshell-init t))

;;;###autoload
(defun doom/eshell-vsplit ()
  (interactive)
  (select-window (split-window-horizontally))
  (setq-local doom-eshell-direction 'right)
  (doom--eshell-init t))

;;;###autoload
(defun doom/eshell (&optional same &rest _)
  (interactive)
  (doom--eshell-init same)
  ;; (if doom-eshell-buffers
  ;;     (let* ((buf (car (reverse doom-eshell-buffers)))
  ;;            (win (get-buffer-window buf)))
  ;;       (if (and win (window-live-p win))
  ;;           (select-window win)
  ;;         (select-window (split-window-vertically doom-eshell-height))
  ;;         (evil-window-move-very-bottom)
  ;;         (switch-to-buffer buf t t)))
  ;;   (doom--eshell-init same))
  )

(defun doom--eshell-init (&optional same)
  (unless same (select-window (split-window)))
  (eshell (max 0 (1- (length doom-eshell-buffers))))
  (unless same
    (evil-window-move-very-bottom)
    (evil-window-set-height doom-eshell-height))
  (set-window-dedicated-p (selected-window) t))

;;;###autoload
(defun doom/eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (doom--eshell-current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " λ " 'face 'font-lock-constant-face)))

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

(provide 'defuns-eshell)
;;; defuns-eshell.el ends here
