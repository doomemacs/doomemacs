;;; lang/org/+attach.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-attach)

;; I believe Org's native attachment system is over-complicated and litters
;; files with metadata I don't want. So I wrote my own, which:
;;
;; + Places attachments in a centralized location (`+org-attach-dir' in
;;   `+org-dir'), using an attach:* link abbreviation.
;; + Use `+org-attach/sync' to index all attachments in `+org-dir' that use the
;;   attach:* abbreviation and delete orphaned ones that are no longer
;;   referenced.
;; + Adds drag-and-drop support for images (with inline image preview)
;; + Adds drag-and-drop support for media files (pdfs, zips, etc) with a
;;   filetype icon and short link.

;; Some commands of interest:
;; + `org-download-screenshot'
;; + `+org-attach/file'
;; + `+org-attach/url'
;; + `+org-attach/sync'

(defvar +org-attach-dir ".attach/"
  "Where to store attachments relative to `+org-dir'.")


;;
;; Plugins
;;

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

  ;; Handle non-image files a little differently. Images should be inserted
  ;; as-is, as image previews. Other files, like pdfs or zips, should be linked
  ;; to, with an icon indicating the type of file.
  (advice-add #'org-download-insert-link :override #'+org-attach*insert-link)

  (defun +org-attach*download-subdir ()
    (when (file-in-directory-p buffer-file-name +org-dir)
      (file-relative-name buffer-file-name +org-dir)))

  (defun +org-attach*download-fullname (path)
    "Write PATH relative to current file."
    (let ((dir (or (if buffer-file-name (file-name-directory buffer-file-name))
                   default-directory)))
      (if (file-in-directory-p dir +org-dir)
          (file-relative-name path dir)
        path)))
  (advice-add #'org-download--dir-2 :override #'ignore)
  (advice-add #'org-download--fullname
              :filter-return #'+org-attach*download-fullname))


;;
;; Bootstrap
;;

(defun +org|init-attach ()
  (setq org-attach-directory (expand-file-name +org-attach-dir +org-dir))

  ;; A shorter link to attachments
  (push (cons "attach" (abbreviate-file-name org-attach-directory)) org-link-abbrev-alist)
  (org-link-set-parameters
   "attach"
   :follow   (lambda (link) (find-file (expand-file-name link org-attach-directory)))
   :complete (lambda (&optional _arg)
               (+org--relpath (+org-link-read-file "attach" org-attach-directory)
                              org-attach-directory))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link org-attach-directory))
                   'org-link
                 'error)))

  (push (car (last (split-string +org-attach-dir "/" t)))
        projectile-globally-ignored-directories)

  ;;
  (after! recentf
    (push (format "%s.+$" (regexp-quote org-attach-directory))
          recentf-exclude)))

