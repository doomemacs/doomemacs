;;; lang/org/+attach.el

;; FIXME Needs to be rewritten
;;
;; Initializes my own org-mode attachment system. I didn't like Org's native
;; one. Mine stores attachments in a global org .attach directory. It also
;; implements drag-and-drop file support and attachment icons. It also treats
;; images specially.
;;
;; To clean up unreferenced attachments, call `doom/org-cleanup-attachments'
(add-hook '+org-init-hook #'+org|init-attach t)

(defun +org|init-attach ()
  (setq org-attach-directory +org-attachment-dir)

  ;; Don't track attachments in recentf or projectile
  (push (format "/%s.+$" (regexp-quote +org-attachment-dir)) recentf-exclude)
  (push ".attach" projectile-globally-ignored-file-suffixes)

  ;; FIXME Use all-the-icons
  ;; (doom-fix-unicode '("FontAwesome" 13) ? ? ? ? ? ? ? ?)
  ;; Drag-and-drop support
  (require 'org-download)
  (setq-default org-download-image-dir +org-attachment-dir
                org-download-heading-lvl nil
                org-download-timestamp "_%Y%m%d_%H%M%S")

  (setq org-download-screenshot-method
        (cond (IS-MAC   "screencapture -i %s")
              (IS-LINUX "maim --opengl -s %s")))

  ;; Write download paths relative to current file
  (advice-add #'org-download--dir-2 :override #'ignore)
  (defun +org*download-fullname (path)
    (file-relative-name path (file-name-directory (buffer-file-name))))
  (advice-add #'org-download--fullname :filter-return #'+org*download-fullname)

  ;; Add another drag-and-drop handler that will handle anything but image files
  (setq dnd-protocol-alist `(("^\\(https?\\|ftp\\|file\\|nfs\\):\\(//\\)?" . doom/org-download-dnd)
                             ,@dnd-protocol-alist))

  ;; keybinds
  ;; (map! :leader :n "oa" (find-file-in! +org-attachment-dir))
  )

