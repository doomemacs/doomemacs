;;; org/org-notebook/autoload.el -*- lexical-binding: t; -*-

(defun +org-notebook--explore-notes (dir)
  (unless (file-directory-p dir)
    (error "Directory doesn't exist: %s" dir))
  (if (fboundp '+evil/neotree)
      (neotree-dir dir)
    (let ((default-directory dir))
      (call-interactively (command-remapping 'find-file)))))

;;;###autoload
(defun +org-notebook/find-major-mode-notes ()
  "Browse org notes in `+org-notebook-code-dir' in neotree, ido, ivy or helm --
whichever is available."
  (interactive)
  (let ((dir (expand-file-name
              (concat (or (cdr (assq major-mode +org-notebook-code-alist))
                          (replace-regexp-in-string
                           "+" "p"
                           (string-remove-suffix "-mode" (symbol-name major-mode))
                           nil t))
                      "/")
              +org-notebook-code-dir)))
    (unless (file-in-directory-p dir +org-notebook-code-dir)
      (error "Invalid location for %s notes: %s"
             major-mode (abbreviate-file-name dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (+org-notebook--explore-notes dir)))

;;;###autoload
(defun +org-notebook/find-project-notes ()
  "Browse org notes in `+org-notebook-project-dir' in neotree, ido, ivy or helm --
whichever is available."
  (interactive)
  (+org-notebook--explore-notes +org-notebook-project-dir))
