;;; editor/format/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +format/org-block (&optional point)
  "Reformat the org src block at POINT with a mode appropriate formatter."
  (interactive (list (point)))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org-mode buffer!"))
  (let ((element (org-element-at-point point)))
    (unless (org-in-src-block-p nil element)
      (user-error "Not in an org src block"))
    (cl-destructuring-bind (beg end _) (org-src--contents-area element)
      (let* ((lang (or (org-element-property :language element)
                       (user-error "Cannot reformat src block without a valid language")))
             (mode (org-src-get-lang-mode lang)))
        (save-excursion
          (if (provided-mode-derived-p mode 'org-mode)
              (user-error "Cannot reformat an org-mode or org-derived src block")
            (let* ((major-mode mode)
                   (after-change-functions
                    ;; HACK: Silence excessive and unhelpful warnings about
                    ;;   'org-element-at-point being used in non-org-mode
                    ;;   buffers'.
                    (remq 'org-indent-refresh-maybe after-change-functions))
                   (apheleia-formatter
                    (or (apheleia--get-formatters)
                        (user-error "No formatter configured for language: %s" lang))))
              (+format-region beg end))))))))

;;;###autoload
(defun +format/org-blocks-in-region (beg end)
  "Calls `+format/org-block' in each src block between BEG and END (inclusive)."
  (interactive (doom-region t))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org-mode buffer!"))
  (unless (and beg end)
    (user-error "No active selection"))
  (let ((n 0))
    (org-block-map
     (lambda ()
       (let ((element (org-element-at-point)))
         (when (and (org-in-src-block-p nil element)
                    (org-element-property :language element))
           (with-demoted-errors "+format/org-block: %s"
             (call-interactively #'+format/org-block))
           (cl-incf n))))
     (save-excursion
       (goto-char beg)
       (when (org-in-src-block-p)
         (org-previous-block 1))
       (point))
     end)
    (message "Formatted %d src block(s)" n)))

;;;###autoload
(defun +format/org-blocks-at-point-or-in-region ()
  "See `+format/org-block' and `+format/org-blocks-in-region'."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+format/org-blocks-in-region
     #'+format/org-block)))

;;; org.ell ends here
