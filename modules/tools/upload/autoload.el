;;; tools/upload/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +upload/put-file (&optional force-p)
  "Upload the current buffer's file to the configured remote."
  (interactive "P")
  (if force-p
      (ssh-deploy-upload-handler-forced)
    (ssh-deploy-upload-handler)))

;;;###autoload
(defun +upload/get-file ()
  "Download the current buffer's file from the configured remote."
  (interactive)
  (ssh-deploy-download-handler))

;;;###autoload
(defun +upload/diff-file ()
  "Open a diff of the local file against the remote."
  (interactive)
  (ssh-deploy-diff-handler))

;;;###autoload
(defun +upload/browse ()
  "Browse the remote directory mapped to this file's directory."
  (interactive)
  (ssh-deploy-browse-remote-handler))

;;;###autoload
(defun +upload/check-remote ()
  "Check if local file exists or if the remote file has changed."
  (interactive)
  (ssh-deploy-remote-changes-handler))
