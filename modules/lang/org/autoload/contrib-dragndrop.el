;;; lang/org/autoload/contrib-dragndrop.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dragndrop)

;;;###autoload
(defun +org-dragndrop-download-dnd-fn (uri action)
  "Handle file links and base64 data uris."
  (if (eq major-mode 'org-mode)
      (+org/attach-file-and-insert-link uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all '+org-dragndrop-download-dnd-fn
                             (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

;;;###autoload
(defun +org-dragndrop-image-fn (protocol link _description)
  "Return the image associated with the current attachment."
  (let ((file (expand-file-name link org-attach-id-dir)))
    (when (and (file-exists-p file) (image-type-from-file-name file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (point-max))))))
