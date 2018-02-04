;; private/default/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defmacro +default--def-browse-in! (name dir &optional prefix)
  (let ((prefix (or prefix (cdr (doom-module-from-path (or load-file-name byte-compile-current-file))))))
    `(defun ,(intern (format "+%s/browse-%s" prefix name)) ()
       (interactive)
       (doom-project-browse ,dir))))

;;;###autoload
(defmacro +default--def-find-in! (name dir &optional prefix)
  (let ((prefix (or prefix (cdr (doom-module-from-path (or load-file-name byte-compile-current-file))))))
    `(defun ,(intern (format "+%s/find-in-%s" prefix name)) ()
       (interactive)
       (doom-project-find-file ,dir))))


;;;###autoload (autoload '+default/browse-project "private/default/autoload/default" nil t)
(+default--def-browse-in! project (doom-project-root))
;; NOTE No need for find-in-project, use `projectile-find-file'

;;;###autoload (autoload '+default/find-in-templates "private/default/autoload/default" nil t)
(+default--def-find-in!   templates +file-templates-dir)
;;;###autoload (autoload '+default/browse-templates "private/default/autoload/default" nil t)
(+default--def-browse-in! templates +file-templates-dir)

;;;###autoload (autoload '+default/find-in-emacsd "private/default/autoload/default" nil t)
(+default--def-find-in!   emacsd doom-emacs-dir)
;;;###autoload (autoload '+default/browse-emacsd "private/default/autoload/default" nil t)
(+default--def-browse-in! emacsd doom-emacs-dir)

;;;###autoload (autoload '+default/find-in-notes "private/default/autoload/default" nil t)
(+default--def-find-in!   notes +org-dir)
;;;###autoload (autoload '+default/browse-notes "private/default/autoload/default" nil t)
(+default--def-browse-in! notes +org-dir)

;;;###autoload (autoload '+default/find-in-snippets "private/default/autoload/default" nil t)
(+default--def-find-in! snippets +default-snippets-dir)
;; NOTE No need for a browse-snippets variant, use `yas-visit-snippet-file'

