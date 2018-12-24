;;; app/notmuch/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =notmuch ()
  "Activate (or switch to) `notmuch' in its workspace."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (condition-case-unless-debug e
      (progn
        (+workspace-switch "*MAIL*" t)
        (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                                   (doom-visible-windows))))
            (select-window (get-buffer-window buf))
          (notmuch-search "tag:inbox"))
        (+workspace/display))
    ('error
     (+notmuch/quit)
     (signal (car e) (cdr e)))))


;;
;; Commands

;;;###autoload
(defun +notmuch/quit ()
  (interactive)
  ;; (+popup/close (get-buffer-window "*notmuch-hello*"))
  (doom-kill-matching-buffers "^\\*notmuch")
  (+workspace/delete "*MAIL*"))

;;;###autoload
(defun +notmuch/update ()
  (interactive)
  (start-process-shell-command
   "notmuch update" nil
   (pcase +notmuch-sync-backend
     (`gmi
      (concat "cd " +notmuch-mail-folder " && gmi push && gmi pull && notmuch new && afew -a -t"))
     (`mbsync
      "mbsync -a && notmuch new && afew -a -t")
     (`mbsync-xdg
      "mbsync -c \"$XDG_CONFIG_HOME\"/isync/mbsyncrc -a && notmuch new && afew -a -t")
     (`offlineimap
      "offlineimap && notmuch new && afew -a -t"))))

;;;###autoload
(defun +notmuch/search-delete ()
  (interactive)
  (notmuch-search-add-tag (list "+trash" "-inbox" "-unread"))
  (notmuch-tree-next-message))

;;;###autoload
(defun +notmuch/tree-delete ()
  (interactive)
  (notmuch-tree-add-tag (list "+trash" "-inbox" "-unread"))
  (notmuch-tree-next-message))

;;;###autoload
(defun +notmuch/search-spam ()
  (interactive)
  (notmuch-search-add-tag (list "+spam" "-inbox" "-unread"))
  (notmuch-search-next-thread))

;;;###autoload
(defun +notmuch/tree-spam ()
  (interactive)
  (notmuch-tree-add-tag (list "+spam" "-inbox" "-unread"))
  (notmuch-tree-next-message))

;;;###autoload
(defun +notmuch/open-message-with-mail-app-notmuch-tree ()
  (interactive)
  (let* ((msg-path (car (plist-get (notmuch-tree-get-message-properties) :filename)))
         (temp (make-temp-file "notmuch-message-" nil ".eml")))
    (shell-command-to-string (format "cp '%s' '%s'" msg-path temp))
    (start-process-shell-command "email" nil (format "xdg-open '%s'" temp))))

;;;###autoload
(defun +notmuch/open-message-with-mail-app-notmuch-show ()
  (interactive)
  (let* ((msg-path (car (plist-get (notmuch-show-get-message-properties) :filename)))
         (temp (make-temp-file "notmuch-message-" nil ".eml")))
    (shell-command-to-string (format "cp '%s' '%s'" msg-path temp))
    (start-process-shell-command "email" nil (format "xdg-open '%s'" temp))))


;;
;; Advice

;;;###autoload
(defun +notmuch*dont-confirm-on-kill-process (orig-fn &rest args)
  "Don't prompt for confirmation when killing notmuch sentinel."
  (let (confirm-kill-processes)
    (apply orig-fn args)))

;; (defun +notmuch*hello-insert-searches (title query-list &rest options)
;;   (widget-insert (propertize title 'face 'org-agenda-structure))
;;   (if (and notmuch-hello-first-run (plist-get options :initially-hidden))
;;       (add-to-list 'notmuch-hello-hidden-sections title))
;;   (let ((is-hidden (member title notmuch-hello-hidden-sections))
;;         (widget-push-button-prefix "")
;;         (widget-push-button-suffix "")
;;         (start (point)))
;;     (if is-hidden
;;         (widget-create 'push-button
;;                        :notify `(lambda (widget &rest ignore)
;;                                   (setq notmuch-hello-hidden-sections
;;                                         (delete ,title notmuch-hello-hidden-sections))
;;                                   (notmuch-hello-update))
;;                        (propertize " +" 'face 'org-agenda-structure))
;;       (widget-create 'push-button
;;                      :notify `(lambda (widget &rest ignore)
;;                                 (add-to-list 'notmuch-hello-hidden-sections
;;                                              ,title)
;;                                 (notmuch-hello-update))
;;                      " -"))
;;     (widget-insert "\n")
;;     (when (not is-hidden)
;;       (let ((searches (apply 'notmuch-hello-query-counts query-list options)))
;;         (when (or (not (plist-get options :hide-if-empty))
;;                   searches)
;;           (widget-insert "\n")
;;           (notmuch-hello-insert-buttons searches)
;;           (indent-rigidly start (point) notmuch-hello-indent))))))

;; (defun +notmuch*hello-insert-saved-searches ()
;;   "Insert the saved-searches section."
;;   (let ((searches (notmuch-hello-query-counts
;;                    (if notmuch-saved-search-sort-function
;;                        (funcall notmuch-saved-search-sort-function
;;                                 notmuch-saved-searches)
;;                      notmuch-saved-searches)
;;                    :show-empty-searches notmuch-show-empty-saved-searches)))
;;     (when searches
;;       (widget-insert (propertize "Notmuch" 'face 'org-agenda-date-today))
;;       (widget-insert "\n\n")
;;       (widget-insert (propertize "Saved searches" 'face 'org-agenda-structure))
;;       (widget-insert "\n\n")
;;       (let ((start (point)))
;;         (notmuch-hello-insert-buttons searches)
;;         (indent-rigidly start (point) notmuch-hello-indent)))))

;; (defun +notmuch*hello-insert-buttons (searches)
;;   (let* ((widest (notmuch-hello-longest-label searches))
;;          (tags-and-width (notmuch-hello-tags-per-line widest))
;;          (tags-per-line (car tags-and-width))
;;          (column-width (cdr tags-and-width))
;;          (column-indent 0)
;;          (count 0)
;;          (reordered-list (notmuch-hello-reflect searches tags-per-line))
;;          ;; Hack the display of the buttons used.
;;          (widget-push-button-prefix "")
;;          (widget-push-button-suffix ""))
;;     ;; dme: It feels as though there should be a better way to
;;     ;; implement this loop than using an incrementing counter.
;;     (mapc (lambda (elem)
;;             ;; (not elem) indicates an empty slot in the matrix.
;;             (when elem
;;               (if (> column-indent 0)
;;                   (widget-insert (make-string column-indent ? )))
;;               (let* ((name (plist-get elem :name))
;;                      (query (plist-get elem :query))
;;                      (oldest-first (case (plist-get elem :sort-order)
;;                                      (newest-first nil)
;;                                      (oldest-first t)
;;                                      (otherwise notmuch-search-oldest-first)))
;;                      (search-type (eq (plist-get elem :search-type) 'tree))
;;                      (msg-count (plist-get elem :count)))
;;                 (widget-insert (format "\n%5s "
;;                                        (notmuch-hello-nice-number msg-count)))
;;                 (widget-create 'push-button
;;                                :notify #'notmuch-hello-widget-search
;;                                :notmuch-search-terms query
;;                                :notmuch-search-oldest-first oldest-first
;;                                :notmuch-search-type search-type
;;                                name)
;;                 (setq column-indent
;;                       (1+ (max 0 (- column-width (length name)))))))
;;             (setq count (1+ count))
;;             (when (eq (% count tags-per-line) 0)
;;               (setq column-indent 0)
;;               (widget-insert "\n")))
;;           reordered-list)

;;     ;; If the last line was not full (and hence did not include a
;;     ;; carriage return), insert one now.
;;     (unless (eq (% count tags-per-line) 0)
;;       (widget-insert "\n"))))
