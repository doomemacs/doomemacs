;;; lang/org/contrib/dragndrop.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dragndrop)

(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  ;; HACK We add these manually so that org-download is truly lazy-loaded
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)

  (after! org
    ;; A shorter link to attachments
    (+org-def-link "download" org-attach-id-dir)
    (setf (alist-get "download" org-link-abbrev-alist nil nil #'equal)
          (abbreviate-file-name org-attach-id-dir)))
  :config
  (setq org-download-image-dir org-attach-id-dir
        org-download-link-format "[[download:%s]]\n"
        org-download-method 'attach
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d_%H%M%S"
        org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))

  ;; Handle non-image files a little differently. Images should be inserted
  ;; as-is, as image previews. Other files, like pdfs or zips, should be linked
  ;; to, with an icon indicating the type of file.
  (defadvice! +org--dragndrop-insert-link-a (_link filename)
    "Produces and inserts a link to FILENAME into the document.

If FILENAME is an image, produce an attach:%s path, otherwise use file:%s (with
an file icon produced by `+org-attach-icon-for')."
    :override #'org-download-insert-link
    (if (looking-back "^[ \t]+" (line-beginning-position))
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (cond ((image-type-from-file-name filename)
           (insert
            (concat
             (if (= org-download-image-html-width 0) ""
               (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
             (if (= org-download-image-latex-width 0) ""
               (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
             (format org-download-link-format
                     (cond ((file-in-directory-p filename org-attach-directory)
                            (file-relative-name filename org-download-image-dir))
                           ((file-in-directory-p filename org-directory)
                            (file-relative-name filename org-directory))
                           (filename)))))
           (org-display-inline-images))
          ((insert
            (format "%s [[./%s][%s]] "
                    (+org-attach-icon-for filename)
                    (file-relative-name filename (file-name-directory buffer-file-name))
                    (file-name-nondirectory (directory-file-name filename)))))))

  (advice-add #'org-download--dir-2 :override #'ignore)
  (defadvice! +org--dragndrop-download-fullname-a (path)
    "Write PATH relative to current file."
    :filter-return #'org-download--fullname
    (let ((dir (or (if buffer-file-name (file-name-directory buffer-file-name))
                   default-directory)))
      (if (file-in-directory-p dir org-directory)
          (file-relative-name path dir)
        path))))
