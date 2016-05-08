;;; module-org-notebook.el

;; This transforms Emacs+org-mode into a notebook application with:
;;   + Custom links for class notes
;;   + Shortcuts for searching org files
;;   + Shortcuts for creating new notes (there's org-capture, but this is suited to my
;;     workflow)
;;   + A simpler attachment system (with auto-deleting support) and drag-and-drop
;;     for images and documents into org files
;;   + A pandoc-powered export system

(add-hook 'org-load-hook 'narf|org-notebook-init t)
(add-hook 'org-load-hook 'narf|org-attach-init t)
(add-hook 'org-load-hook 'narf|org-export-init t)

(defvar org-directory-notebook (expand-file-name "/notebook/" org-directory))
(defvar org-default-notes-file (expand-file-name "inbox.org" org-directory))

(defvar org-attach-directory ".attach/")
(defvar narf-org-export-directory (concat org-directory ".export"))
(defvar narf-org-quicknote-dir (concat org-directory "Inbox/"))

;; Keep track of attachments
(defvar-local narf-org-attachments-list '()
  "A list of attachments for the current buffer")

;; Tell helm to ignore these directories
(after! helm
  (mapc (lambda (r) (add-to-list 'helm-boring-file-regexp-list r))
        (list "\\.attach$" "\\.export$")))


;;
(defun narf|org-notebook-init ()
  ;; (define-org-section! course "Classes"
  ;;   (lambda (path) (substring path 0 (s-index-of " " path))) "%s*.org")
  ;; (exmap "ocl[ass]" 'narf:org-search-course)
  (narf-fix-unicode "FontAwesome" '(? ? ? ? ? ? ? ? ?) 13))

;;
(defun narf|org-attach-init ()
  ;; Drag-and-drop support
  (require 'org-download)
  (setq-default org-download-image-dir org-attach-directory
                org-download-heading-lvl nil
                org-download-timestamp "_%Y%m%d_%H%M%S")

  (when IS-MAC
    (setq org-download-screenshot-method "screencapture -i %s"))

  ;; Write download paths relative to current file
  (defun org-download--dir-2 () nil)
  (defun narf*org-download--fullname (path)
    (f-relative path (f-dirname (buffer-file-name))))
  (advice-add 'org-download--fullname :filter-return 'narf*org-download--fullname)

  ;; Add another drag-and-drop handler that will handle anything but image files
  (setq dnd-protocol-alist `(("^\\(https?\\|ftp\\|file\\|nfs\\):\\(//\\)?" . narf/org-download-dnd)
                             ,@dnd-protocol-alist))

  ;; ...auto-delete attachments once all references to it have been deleted.
  (add-hook 'org-mode-hook 'narf|org-attach-track-init)
  (defun narf|org-attach-track-init ()
    (setq narf-org-attachments-list (narf/org-attachments))
    (add-hook 'after-save-hook 'narf/org-cleanup-attachments nil t)))


;;
(defun narf|org-export-init ()
  "Set up my own exporting system."
  (setq org-export-backends '(ascii html latex md)
        org-export-with-toc t
        org-export-with-author t)

  (require 'ox-pandoc)
  (setq org-pandoc-options '((standalone . t) (mathjax . t) (parse-raw . t)))

  ;; Export to a central directory (why isn't this easier?)
  (unless (file-directory-p narf-org-export-directory)
    (mkdir narf-org-export-directory))
  (defun narf*org-export-output-file-name (args)
    (unless (nth 2 args)
      (setq args (append args (list narf-org-export-directory))))
    args)
  (advice-add 'org-export-output-file-name :filter-args 'narf*org-export-output-file-name))


;;
;; (defvar narf-org-tags '())
;; (defun narf|org-tag-init ()
;;   (async-start
;;    `(lambda ()
;;       (let ((default-directory (narf/project-root))
;;             (data (s-trim (shell-command-to-string "ag --nocolor --nonumbers '^#\\+TAGS:'")))
;;             (alist '()))
;;         (unless (zerop (length data))
;;           (mapc (lambda (l)
;;                   (let* ((parts (s-split ":" l))
;;                          (file (car parts))
;;                          (tags (s-trim (nth 2 parts))))
;;                     (mapc (lambda (tag)
;;                             (setq tag (substring tag 1))
;;                             (unless (assoc tag alist)
;;                               (push (cons tag (list)) alist))
;;                             (push file (cdr (assoc tag alist))))
;;                           (s-split " " tags))))
;;                 (s-lines data))
;;           alist)))
;;    (lambda (_)
;;      (load (concat php-extras-eldoc-functions-file ".el"))
;;      (message "PHP eldoc updated!")))
;;   )

;;
(provide 'module-org-notebook)
;;; module-org-notebook.el ends here
