;;; lang/org/autoload/contrib-roam2.el -*- lexical-binding: t; -*-
;;;###if (featurep! +roam2)

;;; Custom node accessors
;;;###autoload (autoload 'org-roam-node-doom-filetitle "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
      ""))

;;;###autoload (autoload 'org-roam-node-doom-hierarchy "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-doom-filetitle node))
        (separator (propertize " > " 'face 'shadow)))
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      ;; node is a heading with an arbitrary outline path
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp " > ") 'face '(shadow italic))
                 separator title)))))

;;;###autoload (autoload 'org-roam-node-doom-subdirs "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-subdirs ((node org-roam-node))
  "Return subdirectories of `org-roam-directory' in which NODE resides in.
If there's none, return an empty string."
  (if-let ((dirs (thread-first node
                   (org-roam-node-file)
                   (file-relative-name org-roam-directory)
                   (file-name-directory))))
      dirs
    ""))

;;;###autoload (autoload 'org-roam-node-doom-tags "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-tags ((node org-roam-node))
  "Return tags formatted in the same way how they appear in org files.
Treat subdirectories as tags too. If there's no elements to build
the tags of, return an empty string."
  (let ((tags (org-roam-node-tags node))
        (subdirs (org-roam-node-doom-subdirs node)))
    (when tags
      (setq tags (propertize (concat (mapconcat (lambda (s) (concat ":" s)) tags nil) ":")
                             'face 'shadow)))
    (unless (string-empty-p subdirs)
      (setq subdirs (propertize (concat ":" (replace-regexp-in-string "/\\|\\\\" ":" subdirs))
                                'face '(shadow italic))))
    (replace-regexp-in-string ":+" (propertize ":" 'face 'shadow) (concat subdirs tags))))


;;
;;; Advice

;;;###autoload
(defun org-roam-link-follow-link-with-description-a (args)
  "Use a 'roam:X' link's description if X is empty."
  (when (or (string-empty-p (car args))
            (null (car args)))
    (setcar
     args (let ((link (org-element-context)))
            (and (org-element-property :contents-begin link)
                 (org-element-property :contents-end link)
                 (buffer-substring-no-properties
                  (org-element-property :contents-begin link)
                  (org-element-property :contents-end link))))))
  args)

;;;###autoload
(defun org-roam-link-replace-at-point-a (&optional link)
  "Replace \"roam:\" LINK at point with an \"id:\" link."
  (save-excursion
    (save-match-data
      (let* ((link (or link (org-element-context)))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             (desc (and (org-element-property :contents-begin link)
                        (org-element-property :contents-end link)
                        (buffer-substring-no-properties
                         (org-element-property :contents-begin link)
                         (org-element-property :contents-end link))))
             node)
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "roam")
                   ;; Added `desc' ref here
                   (setq node (save-match-data
                                (org-roam-node-from-title-or-alias
                                 (if (string-empty-p path)
                                     desc
                                   path)))))
          (replace-match (org-link-make-string
                          (concat "id:" (org-roam-node-id node))
                          (or desc path))))))))
