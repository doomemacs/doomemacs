;;; ui/workspaces/autoload/ivy.el -*- lexical-binding: t; -*-
;;;###if (modulep! :completion ivy)

;;;###autoload
(defun +workspace--ivy-rich-preview (workspace)
  (if-let (buffers (when-let (workspace (gethash workspace *persp-hash*))
                     (cl-loop for (type . rest) in (persp-window-conf workspace)
                              if (eq type 'buffer)
                              collect (car leaf)
                              else if (eq type 'leaf)
                              append (cl-loop for (type . leaf) in rest
                                              if (eq type 'buffer)
                                              collect (car leaf)))))
      (string-join buffers " ")
    "*No buffers*"))

;;; ivy.el ends here
