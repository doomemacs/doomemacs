;;; emacs/eshell/autoload/eshell.el -*- lexical-binding: t; -*-

;;;###autoload
(defface +eshell-prompt-pwd '((t :inherit eshell-prompt))
  "TODO"
  :group 'eshell)

;;;###autoload
(defface +eshell-prompt-git-branch '((t :inherit font-lock-function-name-face))
  "TODO"
  :group 'eshell)

;;;###autoload
(defface +eshell-prompt-char '((t :inherit font-lock-constant-face))
  "TODO"
  :group 'eshell)


(defvar +eshell-buffers (make-ring 25)
  "List of open eshell buffers.")

(defvar +eshell-buffer-name "*doom eshell*"
  "The name to use for custom eshell buffers. This only affects `+eshell/open',
`+eshell/open-popup' and `+eshell/open-workspace'.")

(defvar +eshell-last-buffer nil
  "TODO")


;;
;; Library
;;

(defun +eshell--add-buffer (buf)
  (ring-remove+insert+extend +eshell-buffers buf))

(defun +eshell--remove-buffer (buf)
  (when-let* ((idx (ring-member +eshell-buffers buf)))
    (ring-remove +eshell-buffers idx)
    t))

(defun +eshell--current-git-branch ()
  (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                              if (string-match-p "^\*" match)
                              collect match))))
    (if (not (eq branch nil))
        (format " [%s]" (substring branch 2))
      "")))

(defun +eshell--buffer (&optional new-p)
  (or (unless new-p
        (cl-loop for buf in (ring-elements +eshell-buffers)
                 if (and (buffer-live-p buf)
                         (not (get-buffer-window buf)))
                 return buf))
      (generate-new-buffer +eshell-buffer-name)))

(defun +eshell--set-window (window &optional flag)
  (when window
    (set-window-parameter window 'no-other-window flag)
    (set-window-parameter window 'visible flag)))

;;;###autoload
(defun +eshell-prompt ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face '+eshell-prompt-pwd)
          (propertize (+eshell--current-git-branch) 'face '+eshell-prompt-git-branch)
          (propertize " λ " 'face '+eshell-prompt-char)))


;;
;; Commands
;;

;;;###autoload
(defun +eshell/open (&optional command)
  "Open eshell in the current buffer."
  (interactive)
  (let ((buf (+eshell--buffer (eq major-mode 'eshell-mode))))
    (switch-to-buffer buf)
    (+eshell--set-window (get-buffer-window buf) t)
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode) (eshell-mode)))
    (when command
      (+eshell-run-command command))))

;;;###autoload
(defun +eshell/open-popup (&optional command)
  "Open eshell in a popup window."
  (interactive)
  (let ((buf (+eshell--buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode) (eshell-mode)))
    (pop-to-buffer buf)
    (+eshell--set-window (get-buffer-window buf) t)
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
  (if-let* ((buf (cl-find-if (lambda (buf) (eq 'eshell-mode (buffer-local-value 'major-mode buf)))
                             (doom-visible-windows)
                             :key #'window-buffer)))
      (select-window (get-buffer-window buf))
    (+eshell/open))
  (when command
    (+eshell-run-command command))
  (+eshell--set-window (selected-window) t)
  (doom/workspace-display))

(defun +eshell-run-command (command)
  (unless (cl-remove-if-not #'buffer-live-p +eshell-buffers)
    (user-error "No living eshell buffers available"))
  (with-current-buffer (car +eshell-buffers)
    (goto-char eshell-last-output-end)
    (when (bound-and-true-p evil-mode)
      (call-interactively #'evil-append-line))
    (insert command)
    (eshell-send-input nil t)))


;;
;; Hooks
;;

;;;###autoload
(defun +eshell|init ()
  "Keep track of eshell buffers."
  (let ((buf (current-buffer)))
    (dolist (buf (ring-elements +eshell-buffers))
      (unless (buffer-live-p buf)
        (+eshell--remove-buffer buf)))
    (+eshell--add-buffer buf)
    (setq +eshell-last-buffer buf)))

;;;###autoload
(defun +eshell|cleanup ()
  "Close window (or workspace) on quit."
  (let ((buf (current-buffer)))
    (when (+eshell--remove-buffer buf)
      (+eshell--set-window (get-buffer-window buf) nil)
      (cond ((and (featurep! :feature workspaces)
                  (string= "eshell" (+workspace-current-name)))
             (+workspace/delete "eshell"))
            ((one-window-p)
             (unless (doom-real-buffer-p (progn (previous-buffer) (current-buffer)))
               (switch-to-buffer (doom-fallback-buffer))))
            ((and (fboundp '+popup-window-p) (+popup-window-p))
             (delete-window))))))


;;
;; Keybinds
;;

;;;###autoload
(defun +eshell/quit-or-delete-char (arg)
  "Delete a character (ahead of the cursor) or quit eshell if there's nothing to
delete."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (eshell-life-is-too-much)
    (delete-char arg)))

(defsubst +eshell--bury-buffer ()
  (unless (switch-to-prev-buffer nil 'bury)
    (switch-to-buffer (doom-fallback-buffer))))

;;;###autoload
(defun +eshell/split-below ()
  "Create a new eshell window below the current one."
  (interactive)
  (select-window (split-window-vertically))
  (+eshell--bury-buffer))

;;;###autoload
(defun +eshell/split-right ()
  "Create a new eshell window to the right of the current one."
  (interactive)
  (select-window (split-window-horizontally))
  (+eshell--bury-buffer))

;; `make-ring'
;; `ring-ref'
;; `ring-empty-p'
;; `ring-remove'

;;;###autoload
(defun +eshell/next ()
  "Switch to the next eshell buffer."
  (interactive)
  (when (ring-empty-p +eshell-buffers)
    (user-error "No eshell buffers are available"))
  (switch-to-buffer (ring-next +eshell-buffers (current-buffer))))

;;;###autoload
(defun +eshell/previous ()
  "Switch to the previous eshell buffer."
  (interactive)
  (when (ring-empty-p +eshell-buffers)
    (user-error "No eshell buffers are available"))
  (switch-to-buffer (ring-previous +eshell-buffers (current-buffer))))

;;;###autoload
(defun +eshell/open-last ()
  "Switch to the last eshell buffer that was open (and is still alive)."
  (interactive)
  (unless (buffer-live-p +eshell-last-buffer)
    (setq +eshell-last-buffer nil)
    (user-error "No last eshell buffer to jump to"))
  (switch-to-buffer +eshell-last-buffer))

;;;###autoload
(defun +eshell/switch (buffer)
  "Interactively switch to another eshell buffer."
  (interactive
   (let ((buffers (cl-remove-if-not (lambda (buf) (eq (buffer-local-value 'major-mode buf) 'eshell-mode))
                                    (delete (current-buffer) (ring-elements +eshell-buffers)))))
     (if (not buffers)
         (user-error "No eshell buffers are available")
       (list (completing-read
              "Eshell buffers"
              (mapcar #'buffer-name buffers)
              #'get-buffer
              'require-match
              nil nil
              (when (eq major-mode 'eshell-mode)
                (buffer-name (current-buffer))))))))
  (if-let* ((window (get-buffer-window buffer)))
      (select-window window)
    (switch-to-buffer buffer)))
