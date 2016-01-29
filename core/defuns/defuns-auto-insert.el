;;; defuns-auto-insert.el
;; for ../core-auto-insert.el

;;;###autoload
(defun narf/auto-insert-snippet (key &optional mode project-only)
  "Auto insert a snippet of yasnippet into new file."
  (interactive)
  (when (if project-only (narf/project-p) t)
    (let ((is-yasnippet-on (not (cond ((functionp yas-dont-activate)
                                       (funcall yas-dont-activate))
                                      ((consp yas-dont-activate)
                                       (some #'funcall yas-dont-activate))
                                      (yas-dont-activate))))
          (snippet (let ((template (cdar (mapcan #'(lambda (table) (yas--fetch table key))
                                                 (yas--get-snippet-tables mode)))))
                     (if template (yas--template-content template) nil))))
      (when (and is-yasnippet-on snippet)
        (yas-expand-snippet snippet)
        (when (and (featurep 'evil) evil-mode)
          (evil-initialize-state 'insert))))))

(provide 'defuns-auto-insert)
;;; defuns-auto-insert.el ends here
