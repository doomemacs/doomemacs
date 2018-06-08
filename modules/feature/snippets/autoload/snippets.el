;;; feature/snippets/autoload/snippets.el -*- lexical-binding: t; -*-

;;;###autoload
(def-setting! :yas-minor-mode (mode)
  "Register a minor MODE with yasnippet so it can have its own snippets
category, if the folder exists."
  (let* ((mode (doom-unquote mode))
         (hookfn (intern (format "+snippets--register-%s" mode))))
    `(after! yasnippet
       (fset ',hookfn (lambda () (+snippets|enable-project-modes ',mode)))
       (add-hook! ,mode #',hookfn))))


;;
;; Commands
;;

;;;###autoload
(defun +snippets/goto-start-of-field ()
  "Go to the beginning of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/goto-end-of-field ()
  "Go to the end of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
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
          (t (call-interactively #'delete-backward-char)))))

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
