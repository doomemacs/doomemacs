;;; lang/org/autoload/contrib-dragndrop.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dragndrop)

;;;###autoload
(defun +org-dragndrop-download-dnd-fn (uri action)
  "TODO"
  (if (eq major-mode 'org-mode)
      (+org-attach/uri uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all '+org-attach-download-dnd
                             (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))
