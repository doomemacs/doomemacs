;;; defuns-eshell.el

(defun doom--eshell-in-prompt-p (&optional offset)
  (>= (- (point) (or offset 0)) (save-excursion (eshell-bol) (point))))

(defun doom--eshell-current-git-branch ()
    (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

;;;###autoload
(defun doom/eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (doom--eshell-current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " $ " 'face 'font-lock-constant-face)))

;;;###autoload
(defun doom/eshell-evil-append ()
  (interactive)
  (goto-char (point-max))
  (call-interactively 'evil-append))

;;;###autoload
(defun doom/eshell-evil-append-maybe ()
  (interactive)
  (if (doom--eshell-in-prompt-p)
      (call-interactively 'evil-insert)
    (doom/eshell-append)))

;;;###autoload
(defun doom/eshell-evil-prepend ()
  (interactive)
  (eshell-bol)
  (call-interactively 'evil-insert))

;;;###autoload
(defun doom/eshell-evil-prepend-maybe ()
  (interactive)
  (if (doom--eshell-in-prompt-p)
      (call-interactively 'evil-insert)
    (doom/eshell-prepend)))

;;;###autoload
(defun doom/eshell-evil-replace-maybe ()
  (interactive)
  (if (doom--eshell-in-prompt-p)
      (call-interactively 'evil-replace)
    (user-error "Cannot edit read-only region")))

;;;###autoload
(defun doom/eshell-evil-replace-state-maybe ()
  (interactive)
  (if (doom--eshell-in-prompt-p)
      (call-interactively 'evil-replace-state)
    (user-error "Cannot edit read-only region")))

(provide 'defuns-eshell)
;;; defuns-eshell.el ends here
