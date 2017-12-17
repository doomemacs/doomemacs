;;; tools/eshell/autoload/eshell.el -*- lexical-binding: t; -*-

(defvar +eshell-buffers ()
  "List of open eshell buffers.")

(defvar +eshell-buffer-name "*doom:eshell*"
  "The name to use for custom eshell buffers. This only affects `+eshell/open',
`+eshell/open-popup' and `+eshell/open-workspace'.")


;; --- Commands ---------------------------

;;;###autoload
(defun +eshell/open (&optional command)
  "Open eshell in the current buffer."
  (interactive)
  (let ((buf (generate-new-buffer +eshell-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode) (eshell-mode)))
    (switch-to-buffer buf)
    (when command
      (+eshell-run-command command))))

;;;###autoload
(defun +eshell/open-popup (&optional command)
  "Open eshell in a popup window."
  (interactive)
  (let ((buf (get-buffer-create +eshell-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode) (eshell-mode)))
    (doom-popup-buffer buf '(:autokill t) t)
    (when command
      (+eshell-run-command command))))

;;;###autoload
(defun +eshell/open-workspace (&optional command)
  "Open eshell in a separate workspace. Requires the (:feature workspaces)
module to be loaded."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (unless (+workspace-get "eshell" t)
    (+workspace/new "eshell"))
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*doom:eshell" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf))
    (+eshell/open))
  (doom/workspace-display)
  (when command
    (+eshell-run-command command)))

(defun +eshell-run-command (command)
  (unless (cl-remove-if-not #'buffer-live-p +eshell-buffers)
    (user-error "No living eshell buffers available"))
  (with-current-buffer (car +eshell-buffers)
    (goto-char eshell-last-output-end)
    (when (bound-and-true-p evil-mode)
      (call-interactively #'evil-append-line))
    (insert command)
    (eshell-send-input nil t)))


;; --- Hooks ------------------------------

;;;###autoload
(defun +eshell|init ()
  "Keep track of eshell buffers."
  (cl-pushnew (current-buffer) +eshell-buffers :test #'eq))

;;;###autoload
(defun +eshell|cleanup ()
  "Close window (or workspace) on quit."
  (setq +eshell-buffers (delete (current-buffer) +eshell-buffers))
  (when (and (featurep! :feature workspaces)
             (string= "eshell" (+workspace-current-name)))
    (+workspace/delete "eshell")))


;; --- Keybindings ------------------------

;;;###autoload
(defun +eshell/quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (eshell-life-is-too-much)
    (delete-char arg)))

(defun +eshell--outside-prompt-p ()
  (< (point) eshell-last-output-end))

(defun +eshell--current-git-branch ()
    (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

;;;###autoload
(defun +eshell/split ()
  (interactive)
  (select-window (split-window-vertically))
  (+eshell/open))

;;;###autoload
(defun +eshell/vsplit ()
  (interactive)
  (select-window (split-window-horizontally))
  (+eshell/open))

;;;###autoload
(defun +eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (+eshell--current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " λ " 'face 'font-lock-constant-face)))

;;;###autoload
(defun +eshell/evil-append ()
  (interactive)
  (goto-char eshell-last-output-end)
  (call-interactively #'evil-append-line))

;;;###autoload
(defun +eshell/evil-append-maybe ()
  (interactive)
  (if (+eshell--outside-prompt-p)
      (+eshell/evil-append)
    (call-interactively #'evil-append)))

;;;###autoload
(defun +eshell/evil-prepend ()
  (interactive)
  (goto-char eshell-last-output-end)
  (call-interactively #'evil-insert))

;;;###autoload
(defun +eshell/evil-prepend-maybe ()
  (interactive)
  (if (+eshell--outside-prompt-p)
      (+eshell/evil-prepend)
    (call-interactively #'evil-insert)))

;;;###autoload
(defun +eshell/evil-replace-maybe ()
  (interactive)
  (if (+eshell--outside-prompt-p)
      (user-error "Cannot edit read-only region")
    (call-interactively #'evil-replace)))

;;;###autoload
(defun +eshell/evil-replace-state-maybe ()
  (interactive)
  (if (+eshell--outside-prompt-p)
      (user-error "Cannot edit read-only region")
    (call-interactively #'evil-replace-state)))

;;;###autoload
(defun +eshell/evil-change ()
  (interactive)
  (when (+eshell--outside-prompt-p)
    (goto-char eshell-last-output-end))
  (call-interactively #'evil-change))

;;;###autoload
(defun +eshell/evil-change-line ()
  (interactive)
  (when (+eshell--outside-prompt-p)
    (goto-char eshell-last-output-end))
  (call-interactively #'evil-change-line))
