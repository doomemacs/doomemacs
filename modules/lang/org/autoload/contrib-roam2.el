;;; lang/org/autoload/contrib-roam2.el -*- lexical-binding: t; -*-
;;;###if (featurep! +roam2)

;;; Custom node accessors

;;;###autoload (autoload 'org-roam-node-doom-filetitle "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  "Return NODE's file level \"#+title:\"."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
      ""))

;;;###autoload (autoload 'org-roam-node-doom-hierarchy "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  "Return NODE's hierarchy, constructed of its file-title, OLP and title.
This will automatically strip out any missing elements or
duplicates from the hierarchy."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-doom-filetitle node)))
    ;; TODO Add 'face based text properties for each type?
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat filetitle " > " title))
      ;; node is a heading with an arbitrary outline path
      (t (concat filetitle " > " (string-join olp " > ") " > " title)))))

;;;###autoload (autoload 'org-roam-node-doom-subdirs "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-subdirs ((node org-roam-node))
  "Return NODE's subdirectories, relative to `org-roam-directory'."
  (if-let ((dirs (thread-first node
                   (org-roam-node-file)
                   (file-relative-name org-roam-directory)
                   (file-name-directory))))
      dirs
    ""))

;;;###autoload (autoload 'org-roam-node-doom-tags "lang/org/autoload/contrib-roam2" nil t)
(cl-defmethod org-roam-node-doom-tags ((node org-roam-node))
  "Return tags formatted in the same way how they appear in org files."
  (mapconcat (lambda (s) (concat ":" s)) (org-roam-node-tags node) nil))
