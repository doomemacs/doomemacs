;;; lang/org/autoload/contrib-dragndrop.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dragndrop)

;;;###autoload
(defun +org-dragndrop-download-dnd (uri action)
  "TODO"
  (if (eq major-mode 'org-mode)
      (+org-attach/uri uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all '+org-attach-download-dnd
                             (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

;;;###autoload
(defun +org-dragndrop*insert-link (_link filename)
    "Produces and inserts a link to FILENAME into the document.

If FILENAME is an image, produce an attach:%s path, otherwise use file:%s (with
an file icon produced by `+org-attach--icon')."
    (if (looking-back "^[ \t]+" (line-beginning-position))
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (cond ((image-type-from-file-name filename)
           (insert
            (concat (if (= org-download-image-html-width 0) ""
                      (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
                    (if (= org-download-image-latex-width 0) ""
                      (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
                    (cond ((file-in-directory-p filename org-attach-directory)
                           (format "[[attach:%s]]" (file-relative-name filename org-attach-directory)))
                          ((file-in-directory-p filename org-directory)
                           (format org-download-link-format (file-relative-name filename org-directory)))
                          ((format org-download-link-format filename)))))
           (org-display-inline-images))
          ((insert
            (format "%s [[./%s][%s]] "
                    (+org-attach--icon filename)
                    (file-relative-name filename (file-name-directory buffer-file-name))
                    (file-name-nondirectory (directory-file-name filename)))))))
