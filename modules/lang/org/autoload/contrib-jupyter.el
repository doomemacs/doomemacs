;;; lang/org/autoload/contrib-jupyter.el -*- lexical-binding: t; -*-
;;;###if (featurep! +jupyter)

;;;###autoload
(defun +org--in-jupyter-src-block-p ()
  (when-let ((info (org-babel-get-src-block-info))
             (lang (car info)))
    (string-prefix-p "jupyter-" lang)))

;;;###autoload
(defun +org--ob-jupyter-initiate-session-a (&rest _)
  (unless (bound-and-true-p jupyter-org-interaction-mode)
    (jupyter-org-interaction-mode)))

;;;###autoload
(defun +org/jupyter-documentation-lookup-handler ()
  (interactive)
  (when (and (+org--in-jupyter-src-block-p)
             (call-interactively #'jupyter-inspect-at-point))
    (display-buffer (help-buffer))
    t))

;;;###autoload
(defun +org--company-box-icons-jupyter (candidate)
  (when (+org--in-jupyter-src-block-p)
    (when-let* ((type (get-text-property 0 'annot candidate))
                (type (string-trim type)))
      (alist-get type +org--company-box-icons-jupyter-alist nil nil
                 #'string-equal))))
