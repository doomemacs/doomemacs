;;; lang/org/contrib/dragndrop.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dragndrop)

(def-package! org-download
  :commands (org-download-dnd org-download-dnd-base64)
  :init
  ;; Add these manually so that org-download is lazy-loaded...
  (add-to-list 'dnd-protocol-alist '("^\\(https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd))
  (add-to-list 'dnd-protocol-alist '("^data:" . org-download-dnd-base64))

  (advice-add #'org-download-enable :override #'ignore)
  :config
  (setq org-download-image-dir org-attach-directory
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
  (advice-add #'org-download-insert-link :override #'+org-dragndrop*insert-link)

  (defun +org-dragndrop*download-fullname (path)
    "Write PATH relative to current file."
    (let ((dir (or (if buffer-file-name (file-name-directory buffer-file-name))
                   default-directory)))
      (if (file-in-directory-p dir org-directory)
          (file-relative-name path dir)
        path)))
  (advice-add #'org-download--dir-2 :override #'ignore)
  (advice-add #'org-download--fullname
              :filter-return #'+org-dragndrop*download-fullname))
