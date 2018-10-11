;;; lang/org/autoload/org-brain.el -*- lexical-binding: t; -*-
;;;###if (featurep! +brain)

;; prettyify org-brain resource links using `all-the-icons` (https://github.com/Kungsgeten/org-brain#all-the-icons)
;;;###autoload
(defun org-brain-insert-resource-icon (link)
  "Insert an icon, based on content of org-mode LINK."
  (insert (format "%s "
                  (cond ((string-prefix-p "http" link)
                         (cond ((string-match "wikipedia\\.org" link)
                                (all-the-icons-faicon "wikipedia-w"))
                               ((string-match "github\\.com" link)
                                (all-the-icons-octicon "mark-github"))
                               ((string-match "vimeo\\.com" link)
                                (all-the-icons-faicon "vimeo"))
                               ((string-match "youtube\\.com" link)
                                (all-the-icons-faicon "youtube"))
                               (t
                                (all-the-icons-faicon "globe"))))
                        ((string-prefix-p "brain:" link)
                         (all-the-icons-fileicon "brain"))
                        (t
                         (all-the-icons-icon-for-file link))))))

;;;###autoload
(add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)

;; prettify org-brain-visualize-mode (https://github.com/Kungsgeten/org-brain#ascii-art-to-unicode)
;;;###autoload
(defun aa2u-buffer ()
  (aa2u (point-min) (point-max)))

;;;###autoload
(add-hook 'org-brain-after-visualize-hook #'aa2u-buffer)
