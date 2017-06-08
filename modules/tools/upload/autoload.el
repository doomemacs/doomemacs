;;; tools/upload/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +upload/local (&optional force-p)
  "TODO"
  (interactive)
  (if force-p
      (ssh-deploy-upload-handler-forced)
    (ssh-deploy-upload-handler)))

;;;###autoload
(defun +upload/remote-download ()
  "TODO"
  (interactive)
  (ssh-deploy-download-handler))

;;;###autoload
(defun +upload/diff ()
  "TODO"
  (interactive)
  (ssh-deploy-diff-handler))

;;;###autoload
(defun +upload/browse ()
  "TODO"
  (interactive)
  (ssh-deploy-browse-remove-handler))

;;;###autoload
(defun +upload/check-remote ()
  "TODO"
  (interactive)
  (ssh-deploy-remote-changes-handler))

