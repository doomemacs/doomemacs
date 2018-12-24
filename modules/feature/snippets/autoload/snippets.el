;;; feature/snippets/autoload/snippets.el -*- lexical-binding: t; -*-

(defun +snippets--remove-p (x y)
  (and (equal (yas--template-key x) (yas--template-key y))
       (file-in-directory-p (yas--template-get-file x) doom-emacs-dir)))

;;;###autoload
(defun +snippets-prompt-private (prompt choices &optional display-fn)
  "Prioritize private snippets over built-in ones if there are multiple
choices.

There are two groups of snippets in Doom Emacs. The built in ones (under
`doom-emacs-dir'; provided by Doom or its plugins) or your private snippets
(outside of `doom-eamcs-dir').

If there are multiple snippets with the same key in either camp (but not both),
you will be prompted to select one.

If there are conflicting keys across the two camps, the built-in ones are
ignored. This makes it easy to override built-in snippets with private ones."
  (when (memq this-command '(yas-expand +snippets/expand-on-region))
    (let* ((gc-cons-threshold doom-gc-cons-upper-limit)
           (choices (cl-remove-duplicates choices :test #'+snippets--remove-p)))
      (if (cdr choices)
          (cl-loop for fn in (cdr (memq '+snippets-prompt-private yas-prompt-functions))
                   if (funcall fn prompt choices display-fn)
                   return it)
        (car choices)))))

;;;###autoload
(defun +snippets/goto-start-of-field ()
  "Go to the beginning of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (active-field (yas--snippet-active-field snippet))
         (position (if (yas--field-p active-field) (yas--field-start active-field) -1)))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/goto-end-of-field ()
  "Go to the end of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (active-field (yas--snippet-active-field snippet))
         (position (if (yas--field-p active-field) (yas--field-end active-field) -1)))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/delete-backward-char (&optional field)
  "Prevents Yas from interfering with backspace deletion."
  (interactive)
  (let ((field (or field (and (overlayp yas--active-field-overlay)
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (unless (and (yas--field-p field)
                 (eq (point) (marker-position (yas--field-start field))))
      (call-interactively #'delete-backward-char))))

;;;###autoload
(defun +snippets/delete-forward-char-or-field (&optional field)
  "Delete forward, or skip the current field if it's empty. This is to prevent
buggy behavior when <delete> is pressed in an empty field."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((not (yas--field-p field))
           (delete-char 1))
          ((and (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          ((eq (point) (marker-position (yas--field-end field))) nil)
          ((delete-char 1)))))

;;;###autoload
(defun +snippets/delete-to-start-of-field (&optional field)
  "Delete to start-of-field."
  (interactive)
  (unless field
    (setq field (and (overlayp yas--active-field-overlay)
                     (overlay-buffer yas--active-field-overlay)
                     (overlay-get yas--active-field-overlay 'yas--field))))
  (when (yas--field-p field)
    (let ((sof (marker-position (yas--field-start field))))
      (when (and field (> (point) sof))
        (delete-region sof (point))))))


;;
;; Hooks

;;;###autoload
(defun +snippets|enable-project-modes (mode &rest _)
  "Automatically enable snippet libraries for project minor modes defined with
`def-project-mode!'."
  (if (symbol-value mode)
      (yas-activate-extra-mode mode)
    (yas-deactivate-extra-mode mode)))


;;
;; Commands

;;;###autoload
(defun +snippets/browse (arg)
  "TODO"
  (interactive "P")
  (doom-project-browse +snippets-dir))

;;;###autoload
(defun +snippets/find-file ()
  "TODO"
  (interactive)
  (if (file-directory-p +snippets-dir)
      (doom-project-find-file +snippets-dir)
    (yas-visit-snippet-file)))

