;;; ui/neotree/autoload.el -*- lexical-binding: t; -*-

;; `neotree-show' and `neotree-find' don't respect the current project, and open
;; neotree in `default-directory'. `+neotree/open' and `neotree/find-this-file'
;; will ensure the neotree pane is always rooted in the project root.

;;;###autoload
(defun +neotree/open ()
  "Open the neotree window in the current project."
  (interactive)
  (require 'neotree)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (neotree-dir (or (doom-project-root)
                     default-directory))))

;;;###autoload
(defun +neotree/find-this-file ()
  "Open the neotree window in the current project, and find the current file."
  (interactive)
  (let ((path buffer-file-name)
        (project-root (or (doom-project-root)
                          default-directory)))
    (require 'neotree)
    (cond ((and (neo-global--window-exists-p)
                (get-buffer-window neo-buffer-name t))
           (neotree-find path project-root)
           (neotree-refresh))
          ((not (and (neo-global--window-exists-p)
                     (equal (file-truename (neo-global--with-buffer neo-buffer--start-node))
                            (file-truename project-root))))
           (neotree-dir project-root)
           (neotree-find path project-root))
          (t
           (neotree-find path project-root)))))

;;;###autoload
(defun +neotree/collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (when-let* ((node (neo-buffer--get-filename-current-line)))
    (if (file-directory-p node)
        (if (neo-buffer--expanded-node-p node)
            (+neotree/collapse)
          (neotree-select-up-node))
      (neotree-select-up-node))))

;;;###autoload
(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (when-let* ((node (neo-buffer--get-filename-current-line)))
    (when (file-directory-p node)
      (neo-buffer--set-expand node nil)
      (neo-buffer--refresh t))
    (when neo-auto-indent-point
      (neo-point-auto-indent))))

;;;###autoload
(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (when-let* ((node (neo-buffer--get-filename-current-line)))
    (cond ((file-directory-p node)
           (neo-buffer--set-expand node t)
           (neo-buffer--refresh t)
           (when neo-auto-indent-point
             (forward-line)
             (neo-point-auto-indent)))
          (t
           (call-interactively #'neotree-enter)))))
