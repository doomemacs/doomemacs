;;; lang/org/autoload/notebook.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-mode-notes-dir ()
  "Return the directory were `major-mode's org notes files are."
  (if-let (name (cdr (assq major-mode +org-notes-code-alist)))
      (expand-file-name (concat name "/") +org-code-notes-dir)
    (let ((mode-name (s-replace "+" "p" (s-chop-suffix "-mode" (symbol-name major-mode)))))
      (expand-file-name (concat mode-name "/") +org-code-notes-dir))))

(defun +org--explore-notes (dir)
  (unless (file-directory-p dir)
    (error "Directory doesn't exist: %s" dir))
  (if (fboundp '+evil/neotree)
      (neotree-dir dir)
    (let ((default-directory dir))
      (call-interactively (command-remapping 'find-file)))))

;;;###autoload
(defun +org/browse-notes-for-major-mode ()
  "Browse org notes in `+org-code-notes-dir' in neotree, ido, ivy or helm --
whichever is available."
  (interactive)
  (let ((dir (+org-mode-notes-dir)))
    (unless (file-in-directory-p dir +org-code-notes-dir)
      (error "Invalid location for %s notes: %s" major-mode (abbreviate-file-name dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (+org--explore-notes dir)))

;;;###autoload
(defun +org/browse-notes-for-project ()
  "Browse org notes in `+org-project-notes-dir' in neotree, ido, ivy or helm --
whichever is available."
  (interactive)
  (+org--explore-notes +org-project-notes-dir))
