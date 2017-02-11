;;; autoload.el

;;;###autoload
(defun +snippets/expand-on-region ()
  "Switch to insert mode when expanding a template via region selection, or go
back to normal mode if there are no fields."
  (interactive)
  (when (evil-visual-state-p)
    (let ((end (region-end)))
      (evil-visual-select
       (region-beginning)
       (if (eq evil-this-type 'line) end (1+ end))
       'inclusive)))
  (yas-insert-snippet)
  (let* ((snippet (first (yas--snippets-at-point)))
         (fields (yas--snippet-fields snippet)))
    (evil-insert-state +1)
    (unless fields (evil-change-state 'normal))))

;;;###autoload
(defun +snippets/goto-start-of-field ()
  "Go to the beginning of the current field."
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/goto-end-of-field ()
  "Go to the end of the current field."
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/delete-backward-char (&optional field)
  "Prevents Yas from interfering with backspace deletion."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((eq (point) (marker-position (yas--field-start field))) nil)
          (t (delete-char -1)))))

;;;###autoload
(defun +snippets/delete-forward-char-or-field (&optional field)
  "Delete forward, or skip the current field if it's empty. This is to prevent
buggy behavior when <delete> is pressed in an empty field."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          ((eq (point) (marker-position (yas--field-end field))) nil)
          (t (delete-char 1)))))

;;;###autoload
(defun +snippets/delete-to-start-of-field (&optional field)
  "Delete to start-of-field."
  (interactive)
  (let* ((field (or field (and yas--active-field-overlay
                               (overlay-buffer yas--active-field-overlay)
                               (overlay-get yas--active-field-overlay 'yas--field))))
         (sof (marker-position (yas--field-start field))))
    (when (and field (> (point) sof))
      (delete-region sof (point)))))

;;;###autoload
(defun +snippets/find-file ()
  "Browse through snippets folder"
  (interactive)
  (projectile-find-file-in-directory (car yas-snippet-dirs)))

;; TODO move this to ivy
;;;###autoload
(defun +snippets/ivy-prompt (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))
