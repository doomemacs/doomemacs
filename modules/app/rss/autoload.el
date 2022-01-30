;;; app/rss/autoload.el -*- lexical-binding: t; -*-

(defvar +rss--wconf nil)

;;;###autoload
(defun =rss ()
  "Activate (or switch to) `elfeed' in its workspace."
  (interactive)
  (if (featurep! :ui workspaces)
      (progn
        (+workspace-switch +rss-workspace-name t)
        (doom/switch-to-scratch-buffer)
        (elfeed)
        (+workspace/display))
    (setq +rss--wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer))
    (elfeed)))

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

;;;###autoload
(defun +rss/copy-link ()
  "Copy current link to clipboard."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (kill-new link)
      (message "Copied %s to clipboard" link))))
;;
;; Hooks

;;;###autoload
(defun +rss-elfeed-wrap-h ()
  "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column'."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq-local truncate-lines nil)
    (setq-local shr-use-fonts nil)
    (setq-local shr-width 85)
    (set-buffer-modified-p nil)))

;;;###autoload
(defun +rss-cleanup-h ()
  "Clean up after an elfeed session. Kills all elfeed and elfeed-org files."
  (interactive)
  ;; `delete-file-projectile-remove-from-cache' slows down `elfeed-db-compact'
  ;; tremendously, so we disable the projectile cache:
  (let (projectile-enable-caching)
    (elfeed-db-compact))
  (let ((buf (previous-buffer)))
    (when (or (null buf) (not (doom-real-buffer-p buf)))
      (switch-to-buffer (doom-fallback-buffer))))
  (let ((search-buffers (doom-buffers-in-mode 'elfeed-search-mode))
        (show-buffers (doom-buffers-in-mode 'elfeed-show-mode))
        kill-buffer-query-functions)
    (dolist (file (bound-and-true-p rmh-elfeed-org-files))
      (when-let (buf (get-file-buffer (expand-file-name file org-directory)))
        (kill-buffer buf)))
    (dolist (b search-buffers)
      (with-current-buffer b
        (remove-hook 'kill-buffer-hook #'+rss-cleanup-h :local)
        (kill-buffer b)))
    (mapc #'kill-buffer show-buffers))
  (if (featurep! :ui workspaces)
      (+workspace/delete +rss-workspace-name)
    (when (window-configuration-p +rss--wconf)
      (set-window-configuration +rss--wconf))
    (setq +rss--wconf nil)))


;;
;; Functions

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
(defun +rss-put-sliced-image-fn (spec alt &optional flags)
  "TODO"
  (letf! (defun insert-image (image &optional alt _area _slice)
           (let ((height (cdr (image-size image t))))
             (insert-sliced-image image alt nil (max 1 (/ height 20.0)) 1)))
    (shr-put-image spec alt flags)))

;;;###autoload
(defun +rss-render-image-tag-without-underline-fn (dom &optional url)
  "TODO"
  (let ((start (point)))
    (shr-tag-img dom url)
    ;; And remove underlines in case images are links, otherwise we get an
    ;; underline beneath every slice.
    (put-text-property start (point) 'face '(:underline nil))))
