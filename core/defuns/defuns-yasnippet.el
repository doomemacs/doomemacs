;;; defuns-yasnippet.el
;; for ../core-yasnippet.el

;;;###autoload
(defun narf|yas-before-expand ()
  "Strip out the shitespace before a line selection."
  (when (narf/evil-visual-line-state-p)
    (setq-local
     yas-selected-text
     (replace-regexp-in-string
      "\\(^ *\\|\n? $\\)" ""
      (buffer-substring-no-properties (region-beginning)
                                      (1- (region-end)))))))

;;;###autoload
(defun narf|yas-after-expand ()
  "Switch to insert mode when expanding a template via backtab, or go back to
normal mode if there are no fields."
  (setq yas-selected-text nil))

;;;###autoload
(defun narf/yas-insert-snippet ()
  "Switch to insert mode when expanding a template via backtab, or go back to
normal mode if there are no fields."
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
(defun narf/yas-goto-start-of-field ()
  "Go to the beginning of a field."
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;;;###autoload
(defun narf/yas-goto-end-of-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun narf/yas-backspace (&optional field)
  "Prevents Yas from stepping on my toes when I use backspace."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((eq (point) (marker-position (yas--field-start field))) nil)
          (t (delete-char -1)))))

;;;###autoload
(defun narf/yas-delete (&optional field)
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
(defun narf/yas-clear-to-sof (&optional field)
  (interactive)
  (let* ((field (or field (and yas--active-field-overlay
                               (overlay-buffer yas--active-field-overlay)
                               (overlay-get yas--active-field-overlay 'yas--field))))
         (sof (marker-position (yas--field-start field))))
    (when (and field (> (point) sof))
      (delete-region sof (point)))))

;; Snippet helpers ;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun narf/yas-find-file ()
  "Browse through snippets folder"
  (interactive)
  (narf/ido-find-file (car narf-snippet-dirs)))

(provide 'defuns-yasnippet)
;;; nlinum-defuns.el ends here
