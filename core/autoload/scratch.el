;;; core/autoload/scratch.el -*- lexical-binding: t; -*-

(defvar doom-scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `doom-scratch-dir'.")

(defvar doom-scratch-dir (concat doom-etc-dir "scratch")
  "Where to save persistent scratch buffers.")

(defvar doom-scratch-buffer-major-mode nil
  "What major mode to use in scratch buffers. This can be one of the
following:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar doom-scratch-buffers nil
  "A list of active scratch buffers.")

(defvar-local doom-scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")

(defvar doom-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")

(defun doom--load-persistent-scratch-buffer (name)
  (let ((scratch-file (expand-file-name (or name doom-scratch-default-file)
                                        doom-scratch-dir)))
    (make-directory doom-scratch-dir t)
    (if (not (file-readable-p scratch-file))
        nil
      (erase-buffer)
      (insert-file-contents scratch-file)
      (set-auto-mode)
      t)))

;;;###autoload
(defun doom-scratch-buffer (&optional mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*doom:scratch (%s)*" project-name)
                        "*doom:scratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (unless buffer
        (setq buffer (current-buffer)
              default-directory directory
              doom-scratch-current-project project-name)
        (setq doom-scratch-buffers (cl-delete-if-not #'buffer-live-p doom-scratch-buffers))
        (cl-pushnew buffer doom-scratch-buffers)
        (doom--load-persistent-scratch-buffer project-name)
        (when (and (eq major-mode 'fundamental-mode)
                   (functionp mode))
          (funcall mode))
        (add-hook 'kill-buffer-hook #'doom|persist-scratch-buffer nil 'local)
        (run-hooks 'doom-scratch-buffer-created-hook))
      buffer)))


;;
;;; Persistent scratch buffer

;;;###autoload
(defun doom|persist-scratch-buffer ()
  "Save the current buffer to `doom-scratch-dir'."
  (write-region
   (point-min) (point-max)
   (expand-file-name (or doom-scratch-current-project doom-scratch-default-file)
                     doom-scratch-dir)))

;;;###autoload
(defun doom|persist-scratch-buffers ()
  "Save all scratch buffers to `doom-scratch-dir'."
  (setq doom-scratch-buffers (cl-delete-if-not #'buffer-live-p doom-scratch-buffers))
  (dolist (buffer doom-scratch-buffers)
    (with-current-buffer buffer
      (doom|persist-scratch-buffer))))

;;;###autoload
(unless noninteractive
  (add-hook 'kill-emacs-hook #'doom|persist-scratch-buffers))


;;
;;; Commands

;;;###autoload
(defun doom/open-scratch-buffer (&optional arg project-p)
  "Opens the (persistent) scratch buffer in a popup.

If passed the prefix ARG, switch to it in the current window.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     (if arg
         #'switch-to-buffer
       #'pop-to-buffer)
     (doom-scratch-buffer
      (cond ((eq doom-scratch-buffer-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null doom-scratch-buffer-major-mode)
             nil)
            ((symbolp doom-scratch-buffer-major-mode)
             doom-scratch-buffer-major-mode))
      default-directory
      (when project-p
        (doom-project-name))))))

;;;###autoload
(defun doom/switch-to-scratch-buffer (&optional project-p)
  "Like `doom/open-scratch-buffer', but switches to it in the current window."
  (interactive)
  (doom/open-scratch-buffer t))

;;;###autoload
(defun doom/open-project-scratch-buffer (&optional arg)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, switch to it in the current window."
  (interactive "P")
  (doom/open-scratch-buffer arg 'project))

;;;###autoload
(defun doom/switch-to-project-scratch-buffer ()
  "Like `doom/open-project-scratch-buffer', but switches to it in the current
window."
  (interactive)
  (doom/open-project-scratch-buffer t))

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
