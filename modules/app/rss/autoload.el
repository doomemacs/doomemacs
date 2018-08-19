;;; app/rss/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =rss ()
  "Activate (or switch to) `elfeed' in its workspace."
  (interactive)
  (call-interactively #'elfeed))

;;;###autoload
(defun +rss/delete-pane ()
  "Delete the *elfeed-entry* split pane."
  (interactive)
  (let* ((buf (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buf)))
    (delete-window window)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

;;;###autoload
(defun +rss/open (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (elfeed-show-entry entry)))

;;;###autoload
(defun +rss/next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line)
    (call-interactively '+rss/open)))

;;;###autoload
(defun +rss/previous ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line -1)
    (call-interactively '+rss/open)))


;;
;; Hooks
;;

;;;###autoload
(defun +rss|elfeed-wrap ()
  "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq-local truncate-lines nil)
    (setq-local shr-width 85)
    (set-buffer-modified-p nil)))

;;;###autoload
(defun +rss|cleanup ()
  "Clean up after an elfeed session. Kills all elfeed and elfeed-org files."
  (interactive)
  (elfeed-db-compact)
  (let ((buf (previous-buffer)))
    (when (or (null buf) (not (doom-real-buffer-p buf)))
      (switch-to-buffer (doom-fallback-buffer))))
  (let ((search-buffers (doom-buffers-in-mode 'elfeed-search-mode))
        (show-buffers (doom-buffers-in-mode 'elfeed-show-mode))
        kill-buffer-query-functions)
    (dolist (file +rss-elfeed-files)
      (when-let* ((buf (get-file-buffer (expand-file-name file org-directory))))
        (kill-buffer buf)))
    (dolist (b search-buffers)
      (with-current-buffer b
        (remove-hook 'kill-buffer-hook #'+rss|cleanup :local)
        (kill-buffer b)))
    (mapc #'kill-buffer show-buffers)))


;;
;; Functions
;;

;;;###autoload
(defun +rss-dead-feeds (&optional years)
  "Return a list of feeds that haven't posted anything in YEARS."
  (let* ((years (or years 1.0))
         (living-feeds (make-hash-table :test 'equal))
         (seconds (* years 365.0 24 60 60))
         (threshold (- (float-time) seconds)))
    (with-elfeed-db-visit (entry feed)
      (let ((date (elfeed-entry-date entry)))
        (when (> date threshold)
          (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
    (cl-loop for url in (elfeed-feed-list)
             unless (gethash url living-feeds)
             collect url)))

;;;###autoload
(defun +rss-put-sliced-image (spec alt &optional flags)
  "TODO"
  (cl-letf (((symbol-function #'insert-image)
             (lambda (image &optional alt _area _slice)
               (let ((height (cdr (image-size image t))))
                 (insert-sliced-image image alt nil (max 1 (/ height 20.0)) 1)))))
    (shr-put-image spec alt flags)))

;;;###autoload
(defun +rss-render-image-tag-without-underline (dom &optional url)
  "TODO"
  (let ((start (point)))
    (shr-tag-img dom url)
    ;; And remove underlines in case images are links, otherwise we get an
    ;; underline beneath every slice.
    (put-text-property start (point) 'face '(:underline nil))))
