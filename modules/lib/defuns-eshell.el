;;; defuns-eshell.el ---

(defun narf--eshell-in-prompt-p (&optional offset)
  (>= (- (point) (or offset 0)) (save-excursion (eshell-bol) (point))))

(defun narf--eshell-current-git-branch ()
    (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

;;;###autoload
(defun narf/eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (narf--eshell-current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " $ " 'face 'font-lock-constant-face)))

;;;###autoload
(defun narf/eshell-evil-append ()
  (interactive)
  (goto-char (point-max))
  (call-interactively 'evil-append))

;;;###autoload
(defun narf/eshell-evil-append-maybe ()
  (interactive)
  (if (narf--eshell-in-prompt-p)
      (call-interactively 'evil-insert)
    (narf/eshell-append)))

;;;###autoload
(defun narf/eshell-evil-prepend ()
  (interactive)
  (eshell-bol)
  (call-interactively 'evil-insert))

;;;###autoload
(defun narf/eshell-evil-prepend-maybe ()
  (interactive)
  (if (narf--eshell-in-prompt-p)
      (call-interactively 'evil-insert)
    (narf/eshell-prepend)))

;;;###autoload
(defun narf/eshell-evil-replace-maybe ()
  (interactive)
  (if (narf--eshell-in-prompt-p)
      (call-interactively 'evil-replace)
    (user-error "Cannot edit read-only region")))

;;;###autoload
(defun narf/eshell-evil-replace-state-maybe ()
  (interactive)
  (if (narf--eshell-in-prompt-p)
      (call-interactively 'evil-replace-state)
    (user-error "Cannot edit read-only region")))

(provide 'defuns-eshell)
;;; defuns-eshell.el ends here
