;;; completion/helm/autoload/helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +helm/projectile-find-file ()
  "Call `helm-find-files' if called from HOME, otherwise
`helm-projectile-find-file'."
  (interactive)
  (call-interactively
   (if (or (file-equal-p default-directory "~")
           (if-let* ((proot (doom-project-root)))
               (file-equal-p proot "~")
             t))
       #'helm-find-files
     #'helm-projectile-find-file)))

;;;###autoload
(defun +helm/workspace-buffer-list ()
  "A version of `helm-buffers-list' with its buffer list restricted to the
current workspace."
  (interactive)
  (unless (modulep! :ui workspaces)
    (user-error "This command requires the :ui workspaces module"))
  (with-no-warnings
    (with-persp-buffer-list nil (helm-buffers-list))))

;;;###autoload
(defun +helm/workspace-mini ()
  "A version of `helm-mini' with its buffer list restricted to the current
workspace."
  (interactive)
  (unless (modulep! :ui workspaces)
    (user-error "This command requires the :ui workspaces module"))
  (with-no-warnings
    (with-persp-buffer-list nil (helm-mini))))


;;
;;; Project search

;;;###autoload
(cl-defun +helm-file-search (&key query in all-files (recursive t) _prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'helm-rg)
  (let ((this-command 'helm-rg)
        (helm-rg-default-directory (or in (doom-project-root) default-directory))
        (helm-rg-default-extra-args
         (delq nil (append (list (when all-files "-z -uu")
                                 (unless recursive "--maxdepth 1")
                                 "--hidden" "-g" "!.git")
                           args))))
    (setq deactivate-mark t)
    (helm-rg (or query
                 (when (use-region-p)
                   (let ((beg (or (bound-and-true-p evil-visual-beginning) (region-beginning)))
                         (end (or (bound-and-true-p evil-visual-end) (region-end))))
                     (when (> (abs (- end beg)) 1)
                       (buffer-substring-no-properties beg end))))
                 ""))))

;;;###autoload
(defun +helm/project-search (&optional arg initial-query directory)
  "Performs a project search from the project root with ripgrep.

ARG (universal argument), include all files, even hidden or compressed ones, in
the search."
  (interactive "P")
  (+helm-file-search
   :query initial-query
   :in directory
   :all-files (and (not (null arg))
                   (listp arg))))

;;;###autoload
(defun +helm/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+helm-file-search
   :query initial-query
   :in default-directory
   :all-files (and (not (null arg))
                   (listp arg))))

;;;###autoload
(defun +helm/jump-list ()
  "TODO"
  (interactive)
  (error "not implemented yet"))
