;;; feature/evil/autoload/neotree.el

;;;###autoload
(defun +evil/neotree ()
  "Toggle the neotree window."
  (interactive)
  (let ((in-neotree (and (neo-global--window-exists-p)
                         (window-live-p neo-global--buffer)
                         (eq (current-buffer) neo-global--buffer)))
        (path buffer-file-name))
    (if in-neotree
        (neotree-hide)
      (let ((project-root (doom-project-root)))
        (unless (and (neo-global--window-exists-p)
                     (equal (file-truename (neo-global--with-buffer neo-buffer--start-node))
                            (file-truename project-root)))
          (neotree-dir project-root))
        (neotree-find path project-root)))))

;;;###autoload
(defun +evil/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (if (file-directory-p node)
        (if (neo-buffer--expanded-node-p node)
            (+evil/neotree-collapse)
          (neotree-select-up-node))
      (neotree-select-up-node))))

;;;###autoload
(defun +evil/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (when (file-directory-p node)
      (neo-buffer--set-expand node nil)
      (neo-buffer--refresh t))
    (when neo-auto-indent-point
      (neo-point-auto-indent))))

;;;###autoload
(defun +evil/neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (if (file-directory-p node)
        (progn
          (neo-buffer--set-expand node t)
          (neo-buffer--refresh t)
          (when neo-auto-indent-point
            (next-line)
            (neo-point-auto-indent)))
      (call-interactively 'neotree-enter))))
