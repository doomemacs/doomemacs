;;; tools/neotree/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +neotree/toggle ()
  "Toggle the neotree window."
  (interactive)
  (let ((path buffer-file-name)
        (project-root (doom-project-root)))
    (require 'neotree)
    (cond ((and (neo-global--window-exists-p)
                (get-buffer-window neo-buffer-name t))
           (neotree-hide)
           (neotree-find path project-root))
          ((not (and (neo-global--window-exists-p)
                     (equal (file-truename (neo-global--with-buffer neo-buffer--start-node))
                            (file-truename project-root))))
           (neotree-dir project-root))
          (t (neotree-find path project-root)))))

;;;###autoload
(defun +neotree/collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (if (file-directory-p node)
        (if (neo-buffer--expanded-node-p node)
            (+neotree/collapse)
          (neotree-select-up-node))
      (neotree-select-up-node))))

;;;###autoload
(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (when (file-directory-p node)
      (neo-buffer--set-expand node nil)
      (neo-buffer--refresh t))
    (when neo-auto-indent-point
      (neo-point-auto-indent))))

;;;###autoload
(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (cond ((file-directory-p node)
           (neo-buffer--set-expand node t)
           (neo-buffer--refresh t)
           (when neo-auto-indent-point
             (forward-line)
             (neo-point-auto-indent)))
          (t
           (call-interactively #'neotree-enter)))))
