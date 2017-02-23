;;; lang/org/autoload/capture.el

;;;###autoload
(defun +org/capture (&optional template string)
  "Run `org-capture' in a new, disposable popup frame."
  (interactive)
  (let ((org-capture-entry (org-capture-select-template template)))
    (cond ((equal org-capture-entry "C")
           (find-file (expand-file-name "module-org-notes.el" doom-modules-dir))
           (re-search-forward "^\\s-+(setq org-capture-templates" (point-max) t)
           (recenter))
          ((not (equal org-capture-entry "q"))
           (let ((frame (make-frame '((name . "org-capture") (height . 15) (width . 80)))))
             (with-selected-frame frame
               (if string
                   (org-capture-string string)
                 (org-capture))))))))

