;;; org/org-attach/config.el -*- lexical-binding: t; -*-

(defvar +org-attach-dir (expand-file-name ".attach/" +org-dir)
  "Where to store attachments (relative to current org file).")


(add-hook '+org-init-hook #'+org|init-attach t)

;; FIXME This module is broken and needs to be rewritten.
;;
;; I believe Org's native attachment system is over-complicated and litters
;; files with metadata I don't want.
;;
;; This installs my own attachment system. It:
;;
;; + Centralizes attachment in a global location,
;; + Adds drag-and-drop file support
;; + TODO ...with attachment icons, and
;; + TODO Offers an attachment management system.

(def-package! org-download
  :config
  (setq-default org-download-image-dir +org-attach-dir
                org-download-heading-lvl nil
                org-download-timestamp "_%Y%m%d_%H%M%S")
  (setq org-download-screenshot-method
        (cond (IS-MAC   "screencapture -i %s")
              (IS-LINUX "maim --opengl -s %s")))

  ;; Write download paths relative to current file
  (defun +org-attach*download-fullname (path)
    (file-relative-name path (file-name-directory (buffer-file-name))))
  (advice-add #'org-download--dir-2 :override #'ignore)
  (advice-add #'org-download--fullname
              :filter-return #'+org-attach*download-fullname))

;;
(defun +org-attach|init ()
  (setq org-attach-directory +org-attach-directory)

  (push ".attach" projectile-globally-ignored-file-suffixes)
  (after! recentf
    (push (format "/%s.+$" (regexp-quote +org-attach-dir))
          recentf-exclude))

  (require 'org-download)

  ;; Add another drag-and-drop handler that will handle anything but image files
  (push '("^\\(https?\\|ftp\\|file\\|nfs\\):\\(//\\)?" . +org-attach-download-dnd) dnd-protocol-alist))
