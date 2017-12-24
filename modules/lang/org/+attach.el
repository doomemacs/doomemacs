;;; lang/org/+attach.el -*- lexical-binding: t; -*-

;; I believe Org's native attachment system is over-complicated and litters
;; files with metadata I don't want. So I wrote my own, which:
;;
;; + Causes attachments to be placed in a centralized location,
;; + Adds drag-and-drop support for images (with inline image preview)
;; + Adds drag-and-drop support for media files (pdfs, zips, etc) with a
;;   filetype icon and short link.
;; + TODO Offers an attachment management system.

;; Some commands of interest:
;; + `org-download-screenshot'
;; + `+org-attach/file'
;; + `+org-attach/url'
;; + :org [FILE/URL]

(defvar +org-attach-dir ".attach/"
  "Where to store attachments (relative to current org file).")


(def-package! org-download
  :commands (org-download-dnd org-download-dnd-base64)
  :init
  ;; Add these myself, so that org-download is lazy-loaded...
  (setq dnd-protocol-alist
        `(("^\\(https?\\|ftp\\|file\\|nfs\\):" . +org-attach-download-dnd)
          ("^data:" . org-download-dnd-base64)
          ,@dnd-protocol-alist))

  (advice-add #'org-download-enable :override #'ignore)
  :config
  (setq-default org-download-image-dir org-attach-directory
                org-download-heading-lvl nil
                org-download-timestamp "_%Y%m%d_%H%M%S")

  (setq org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))

  ;; Ensure that relative inline image paths are relative to the attachment folder.
  (advice-add #'org-display-inline-images :around #'+org-attach*relative-to-attach-dir)

  ;; Handle non-image files a little differently. Images should be inserted
  ;; as-is, as image previews. Other files, like pdfs or zips, should be linked
  ;; to, with an icon indicating the type of file.
  (advice-add #'org-download-insert-link :override #'+org-attach*insert-link)

  (defun +org-attach*download-subdir ()
    (when (file-in-directory-p buffer-file-name +org-dir)
      (file-relative-name buffer-file-name +org-dir)))

  ;; Write download paths relative to current file
  (defun +org-attach*download-fullname (path)
    (file-relative-name path (file-name-directory buffer-file-name)))
  (advice-add #'org-download--dir-2 :override #'ignore)
  (advice-add #'org-download--fullname
              :filter-return #'+org-attach*download-fullname))

;;
(after! org
  (setq org-attach-directory (expand-file-name +org-attach-dir +org-dir))

  (push (car (last (split-string +org-attach-dir "/" t)))
        projectile-globally-ignored-directories)

  (after! recentf
    (push (format "%s.+$" (regexp-quote org-attach-directory))
          recentf-exclude)))

