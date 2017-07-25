;;; tools/eshell/autoload/eshell.el -*- lexical-binding: t; -*-

(require 'eshell)

;;;###autoload
(defun +eshell/run ()
  "Open eshell in the current buffer."
  (interactive)
  (let ((buf (generate-new-buffer eshell-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode) (eshell-mode)))
    (pop-to-buffer-same-window buf)))

;;;###autoload
(defun +eshell/popup ()
  "Open eshell in a popup window."
  (interactive)
  (let ((buf (get-buffer-create "*eshell:popup*")))
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode) (eshell-mode)))
    (doom-popup-buffer buf)))

;;;###autoload
(defun +eshell/tab ()
  "Open eshell in a separate workspace. Requires the (:feature workspaces)
module to be loaded."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (unless (+workspace-get "eshell" t)
    (+workspace/new "eshell"))
  (if-let (buf (cl-find-if (lambda (it) (string-match-p "^\\*eshell" (buffer-name (window-buffer it))))
                           (doom-visible-windows)))
      (select-window (get-buffer-window buf))
    (+eshell/run))
  (doom/workspace-display))

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
  (+eshell:run))

;;;###autoload
(defun +eshell/vsplit ()
  (interactive)
  (select-window (split-window-horizontally))
  (+eshell:run))

;;;###autoload
(defun +eshell/prompt ()
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
