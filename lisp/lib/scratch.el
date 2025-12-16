;;; lisp/lib/scratch.el -*- lexical-binding: t; -*-

(defvar doom-scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `doom-scratch-dir'.")

(defvar doom-scratch-dir (concat doom-data-dir "scratch")
  "Where to save persistent scratch buffers.")

(defvar doom-scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar doom-scratch-buffers nil
  "A list of active scratch buffers.")

(defvar doom-scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put 'doom-scratch-current-project 'permanent-local t)

(defvar doom-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")

(defun doom--scratch-buffer-initial-mode ()
  "Return the initial major mode for a new scratch buffer.
Respects `doom-scratch-initial-major-mode' configuration."
  (cond ((eq doom-scratch-initial-major-mode t)
         (unless (or buffer-read-only
                     (derived-mode-p 'special-mode)
                     (string-match-p "^ ?\\*" (buffer-name)))
           major-mode))
        ((null doom-scratch-initial-major-mode)
         nil)
        ((symbolp doom-scratch-initial-major-mode)
         doom-scratch-initial-major-mode)))


(defun doom--load-persistent-scratch-buffer (project-name)
  (setq-local doom-scratch-current-project
              (or project-name
                  doom-scratch-default-file))
  (let ((smart-scratch-file
         (expand-file-name (concat doom-scratch-current-project ".el")
                           doom-scratch-dir)))
    (make-directory doom-scratch-dir t)
    (when (file-readable-p smart-scratch-file)
      (message "Reading %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents smart-scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

;;;###autoload
(defun doom-scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*doom:scratch (%s)*" project-name)
                        "*doom:scratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (doom--load-persistent-scratch-buffer project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) doom-scratch-buffers)
      (add-transient-hook! 'doom-switch-buffer-hook (doom-persist-scratch-buffers-h))
      (add-transient-hook! 'doom-switch-window-hook (doom-persist-scratch-buffers-h))
      (add-hook 'kill-buffer-hook #'doom-persist-scratch-buffer-h nil 'local)
      (run-hooks 'doom-scratch-buffer-created-hook)
      (current-buffer))))


;;
;;; Persistent scratch buffer

;;;###autoload
(defun doom-persist-scratch-buffer-h ()
  "Save the current buffer to `doom-scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (with-temp-file
        (expand-file-name (concat (or doom-scratch-current-project
                                      doom-scratch-default-file)
                                  ".el")
                          doom-scratch-dir)
      (prin1 (list content
                   point
                   mode)
             (current-buffer)))))

;;;###autoload
(defun doom-persist-scratch-buffers-h ()
  "Save all scratch buffers to `doom-scratch-dir'."
  (setq doom-scratch-buffers
        (cl-delete-if-not #'buffer-live-p doom-scratch-buffers))
  (dolist (buffer doom-scratch-buffers)
    (with-current-buffer buffer
      (doom-persist-scratch-buffer-h))))

;;;###autoload
(defun doom-persist-scratch-buffers-after-switch-h ()
  "Kill scratch buffers when they are no longer visible, saving them to disk."
  (unless (cl-some #'get-buffer-window doom-scratch-buffers)
    (mapc #'kill-buffer doom-scratch-buffers)
    (remove-hook 'doom-switch-buffer-hook #'doom-persist-scratch-buffers-after-switch-h)))

;;;###autoload
(unless noninteractive
  (add-hook 'kill-emacs-hook #'doom-persist-scratch-buffers-h))


;;
;;; Commands

(defvar projectile-enable-caching)
;;;###autoload
(defun doom/open-scratch-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     (if same-window-p
         #'switch-to-buffer
       #'pop-to-buffer)
     (doom-scratch-buffer
      arg
      (doom--scratch-buffer-initial-mode)
      default-directory
      (when project-p
        (doom-project-name))))))

;;;###autoload
(defun doom/switch-to-scratch-buffer (&optional arg project-p)
  "Like `doom/open-scratch-buffer', but switches to it in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (doom/open-scratch-buffer arg project-p 'same-window))

;;;###autoload
(defun doom/open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (doom/open-scratch-buffer arg 'project same-window-p))

;;;###autoload
(defun doom/switch-to-project-scratch-buffer (&optional arg)
  "Like `doom/open-project-scratch-buffer', but switches to it in the current
window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (doom/open-project-scratch-buffer arg 'same-window))

;;;###autoload
(defun doom/toggle-scratch-buffer (&optional arg project-p same-window-p)
  "Toggle a persistent scratch buffer.

If the scratch buffer is visible, close its window. Otherwise, open it.
If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, use a project-associated scratch buffer."
  (interactive "P")
  (let* ((project-name (when project-p (doom-project-name)))
         (buf (doom-scratch-buffer arg (doom--scratch-buffer-initial-mode)
                                   default-directory project-name))
         (win (get-buffer-window buf)))
    (if win
        (delete-window win)
      (funcall (if same-window-p #'switch-to-buffer #'pop-to-buffer) buf))))

;;;###autoload
(defun doom/toggle-project-scratch-buffer (&optional arg same-window-p)
  "Toggle the project-associated scratch buffer.

If the scratch buffer is visible, close its window. Otherwise, open it.
If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (doom/toggle-scratch-buffer arg 'project same-window-p))

;;;###autoload
(defun doom/revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*doom:scratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (doom--load-persistent-scratch-buffer doom-scratch-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun doom/delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `doom-scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory doom-scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name doom-scratch-dir)))
    (make-directory doom-scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " doom-scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

(provide 'doom-lib '(scratch))
;;; scratch.el ends here
