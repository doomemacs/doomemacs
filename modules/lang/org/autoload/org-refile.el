;;; lang/org/autoload/org-refile.el -*- lexical-binding: t; -*-

;; REVIEW These are all proof-of-concept. Refactor me!

;;;###autoload
(defun +org/refile-to-current-file (arg &optional file)
  "TODO"
  (interactive "P")
  (let ((org-refile-targets `((,file :maxlevel . 10)))
        (org-refile-use-outline-path nil)
        (org-refile-keep arg)
        current-prefix-arg)
    (call-interactively #'org-refile)))

;;;###autoload
(defun +org/refile-to-file (arg file)
  "Refile current heading to a particular org file."
  (interactive
   (list current-prefix-arg
         (read-file-name "Select file to refile to: "
                         default-directory
                         buffer-file-name
                         t nil
                         (lambda (f) (string-match-p "\\.org$" f)))))
  (+org/refile-to-current-file arg file))

;;;###autoload
(defun +org/refile-to-other-window (arg)
  "TODO"
  (interactive "P")
  (let ((org-refile-keep arg)
        org-refile-targets
        current-prefix-arg)
    (dolist (win (delq (selected-window) (window-list)))
      (with-selected-window win
        (and (eq major-mode 'org-mode)
             buffer-file-name
             (cl-pushnew (cons buffer-file-name (cons :maxlevel 10))
                         org-refile-targets))))
    (call-interactively #'org-refile)))

;;;###autoload
(defun +org/refile-to-other-buffer (arg)
  "TODO"
  (interactive "P")
  (let ((org-refile-keep arg)
        org-refile-targets
        current-prefix-arg)
    (dolist (buf (delq (current-buffer) (doom-buffers-in-mode 'org-mode)))
      (with-current-buffer buf
        (and buffer-file-name
             (cl-pushnew (cons buffer-file-name (cons :maxlevel 10))
                         org-refile-targets))))
    (call-interactively #'org-refile)))

;;;###autoload
(defun +org/refile-to-running-clock (arg)
  "TODO"
  (interactive "P")
  (unless (bound-and-true-p org-clock-current-task)
    (user-error "No active clock to refile to"))
  (let ((org-refile-keep arg))
    (org-refile 2)))

;;;###autoload
(defun +org/refile-to-last-location (arg)
  "TODO"
  (interactive "P")
  (or (assoc (plist-get org-bookmark-names-plist :last-refile)
             bookmark-alist)
      (user-error "No saved location to refile to"))
  (let ((org-refile-keep arg)
        (completing-read-function
         (lambda (_p _coll _pred _rm _ii _h default &rest _)
           default)))
    (org-refile)))

(defvar org-after-refile-insert-hook)
;; Inspired by org-teleport and alphapapa/alpha-org
;;;###autoload
(defun +org/refile-to-visible ()
  "Refile current heading as first child of visible heading selected with Avy."
  (interactive)
  (when-let (marker (+org-headline-avy))
    (let* ((buffer (marker-buffer marker))
           (filename
            (buffer-file-name (or (buffer-base-buffer buffer)
                                  buffer)))
           (heading
            (org-with-point-at marker
              (org-get-heading 'no-tags 'no-todo)))
           ;; Won't work with target buffers whose filename is nil
           (rfloc (list heading filename nil marker))
           (org-after-refile-insert-hook (cons #'org-reveal org-after-refile-insert-hook)))
      (org-refile nil nil rfloc))))
