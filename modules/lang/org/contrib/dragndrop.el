;;; lang/org/contrib/dragndrop.el -*- lexical-binding: t; -*-
;;;###if (modulep! +dragndrop)

(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-clipboard
  org-download-dnd-base64
  :init
  ;; HACK We add these manually so that org-download is truly lazy-loaded
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . org-download-dnd)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)

  (after! org
    ;; A shorter link to attachments
    (+org-define-basic-link "download" (lambda () (or org-download-image-dir org-attach-id-dir "."))
      :image-data-fun #'+org-image-file-data-fn
      :requires 'org-download))
  :config
  (unless org-download-image-dir
    (setq org-download-image-dir org-attach-id-dir))
  (setq org-download-method 'attach
        org-download-timestamp "_%Y%m%d_%H%M%S"
        org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -s %s")
                     ((executable-find "scrot") "scrot -s %s")
                     ((executable-find "gnome-screenshot") "gnome-screenshot -a -f %s"))))

        org-download-heading-lvl nil
        org-download-link-format "[[download:%s]]\n"
        org-download-annotate-function (lambda (_link) "")
        org-download-link-format-function
        (lambda (filename)
          (if (eq org-download-method 'attach)
              (format "[[attachment:%s]]\n"
                      (org-link-escape
                       (file-relative-name filename (org-attach-dir))))
            ;; Handle non-image files a little differently. Images should be
            ;; inserted as normal with previews. Other files, like pdfs or zips,
            ;; should be linked to, with an icon indicating the type of file.
            (format (concat (unless (image-type-from-file-name filename)
                              (concat (+org-attach-icon-for filename)
                                      " "))
                            org-download-link-format)
                    (org-link-escape
                     (funcall org-download-abbreviate-filename-function filename)))))
        org-download-abbreviate-filename-function
        (lambda (path)
          (if (file-in-directory-p path org-download-image-dir)
              (file-relative-name path org-download-image-dir)
            path)))

  (defadvice! +org--fix-org-download-delete-a (fn beg end &optional times)
    "Fix `org-download-delete' for a non-standard `org-download-link-format'."
    :around #'org-download--delete
    (save-excursion
      (save-match-data
        (goto-char beg)
        (let ((times (or times most-positive-fixnum))
              (linkname
               (or (and (string-match "\\[\\[\\(\\w+\\):" org-download-link-format)
                        (match-string 1 org-download-link-format))
                   "file")))
          (while (and (>= (cl-decf times) 0)
                      (re-search-forward (format "\\[\\[%s:\\([^]]*\\)\\]\\]"
                                                 (regexp-quote linkname))
                                         end t))
            (let ((str (match-string-no-properties 2)))
              (delete-region beg (match-end 0))
              (when (file-exists-p str)
                (delete-file str))))))))

  (defadvice! +org--dragndrop-then-display-inline-images-a (_link filename)
    :after #'org-download-insert-link
    (when (image-type-from-file-name filename)
      (save-excursion
        (org-display-inline-images
         t t
         (progn (org-back-to-heading-or-point-min t) (point))
         (progn (org-end-of-subtree t t)
                (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                (point)))))))
