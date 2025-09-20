;;; lang/org/autoload/contrib-roam.el -*- lexical-binding: t; -*-
;;;###if (or (modulep! +roam) (modulep! +roam2))

;;; Custom node accessors
;;;###autoload (autoload 'org-roam-node-doom-filetitle "lang/org/autoload/contrib-roam" nil t)
(cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-node-file-title node))
      ""))

;;;###autoload (autoload 'org-roam-node-doom-hierarchy "lang/org/autoload/contrib-roam" nil t)
(cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-doom-filetitle node))
        (separator (propertize org-eldoc-breadcrumb-separator 'face 'shadow)))
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      ;; node is a heading with an arbitrary outline path
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp separator) 'face '(shadow italic))
                 separator title)))))

;;;###autoload (autoload 'org-roam-node-doom-subdirs "lang/org/autoload/contrib-roam" nil t)
(cl-defmethod org-roam-node-doom-subdirs ((node org-roam-node))
  "Return subdirectories of `org-roam-directory' in which NODE resides in.
If there's none, return an empty string."
  (thread-first
    node
    (org-roam-node-file)
    (file-relative-name org-roam-directory)
    (file-name-directory)))

;;;###autoload (autoload 'org-roam-node-doom-tags "lang/org/autoload/contrib-roam" nil t)
(cl-defmethod org-roam-node-doom-tags ((node org-roam-node))
  "Return tags formatted in the same way how they appear in org files."
  (cl-remove-if (doom-rpartial
                 #'member (delq
                           nil (append
                                (list (bound-and-true-p org-archive-tag)
                                      (bound-and-true-p org-attach-auto-tag))
                                (bound-and-true-p org-num-skip-tags))))
                (org-roam-node-tags node)))

;;;###autoload (autoload 'org-roam-node-doom-type "lang/org/autoload/contrib-roam" nil t)
(cl-defmethod org-roam-node-doom-type ((node org-roam-node))
  "Return the directory relative to `org-roam-directory' as a note's \"type\"."
  (when-let (dir (thread-first
                   node
                   (org-roam-node-file)
                   (file-relative-name org-roam-directory)
                   (file-name-directory)))
    (directory-file-name dir)))


;;
;;; Hooks

;;;###autoload
(defun +org-roam-manage-backlinks-buffer-h ()
  "Open or close roam backlinks buffer depending on visible org-roam buffers.

Intended to be added to `doom-switch-buffer-hook' in `org-roam-find-file-hook'.
Controlled by `+org-roam-open-buffer-on-find-file'."
  (when (and +org-roam-auto-backlinks-buffer
             (not org-roam-capture--node)  ; not for roam capture buffers
             (not org-capture-mode)        ; not for capture buffers
             (not (bound-and-true-p +popup-buffer-mode)))
    (let ((visible-p (eq 'visible (org-roam-buffer--visibility))))
      (if (cl-some #'org-roam-buffer-p (doom-visible-buffers))
          (unless visible-p
            (org-roam-buffer-toggle))
        (when visible-p
          (org-roam-buffer-toggle))
        (unless (doom-buffers-in-mode 'org-mode)
          (remove-hook 'doom-switch-buffer-hook #'+org-roam-manage-backlinks-buffer-h))))))


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
