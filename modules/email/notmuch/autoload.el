;;; email/notmuch/autoload.el -*- lexical-binding: t; -*-

(defvar +notmuch-workspace-name "*notmuch*"
  "Name of the workspace created by `=notmuch', dedicated to notmuch.")

;;;###autoload
(defun =notmuch ()
  "Activate (or switch to) `notmuch' in its workspace."
  (interactive)
  (condition-case-unless-debug e
      (progn
        (when (modulep! :ui workspaces)
          (+workspace-switch +notmuch-workspace-name t))
        (if-let* ((win (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                                   (doom-visible-windows))))
            (select-window win)
          (funcall +notmuch-home-function))
        (when (modulep! :ui workspaces)
          (+workspace/display)))
    ('error
     (+notmuch/quit)
     (signal (car e) (cdr e)))))


;;
;; Commands

;;;###autoload
(defun +notmuch/quit ()
  "TODO"
  (interactive)
  ;; (+popup/close (get-buffer-window "*notmuch-hello*"))
  (doom-kill-matching-buffers "^\\*notmuch")
  (when (modulep! :ui workspaces)
    (+workspace/kill +notmuch-workspace-name)))

(defun +notmuch-get-sync-command ()
  "Return a shell command string to synchronize your notmuch mail with."
  (let* ((afew-cmd "afew -a -t")
         (sync-cmd
          (pcase +notmuch-sync-backend
            (`gmi
             (concat "cd " +notmuch-mail-folder " && gmi sync && notmuch new"))
            ((or `mbsync
                 `mbsync-xdg) ; DEPRECATED `mbsync-xdg' is now just `mbsync'
             (format "mbsync %s -a && notmuch new"
                     (if-let (config-file
                              (doom-glob (or (getenv "XDG_CONFIG_HOME")
                                             "~/.config")
                                         "isync/mbsyncrc"))
                         (format "-c %S" (car config-file))
                       "")))
            (`offlineimap
             "offlineimap && notmuch new")
            ((and (pred stringp) it) it)
            (_ (user-error "Invalid notmuch backend specified: %S"
                           +notmuch-sync-backend)))))
    (if (modulep! +afew)
        (format "%s && %s" sync-cmd afew-cmd)
      sync-cmd)))

;;;###autoload
(defun +notmuch/update ()
  "Sync notmuch emails with server."
  (interactive)
  (let ((compilation-buffer-name-function (lambda (_) (format "*notmuch update*"))))
   (with-current-buffer (compile (+notmuch-get-sync-command))
     (add-hook
      'compilation-finish-functions
      (lambda (buf status)
        (if (equal status "finished\n")
            (progn
              (delete-windows-on buf)
              (bury-buffer buf)
              (notmuch-refresh-all-buffers)
              (message "Notmuch sync successful"))
          (user-error "Failed to sync notmuch data")))
      nil
      'local))))

;;;###autoload
(defun +notmuch/search-delete ()
  (interactive)
  (notmuch-search-add-tag +notmuch-delete-tags)
  (notmuch-tree-next-message))

;;;###autoload
(defun +notmuch/tree-delete ()
  (interactive)
  (notmuch-tree-add-tag +notmuch-delete-tags)
  (notmuch-tree-next-message))

;;;###autoload
(defun +notmuch/show-delete ()
  "Mark email for deletion in notmuch-show"
  (interactive)
  (notmuch-show-add-tag +notmuch-delete-tags)
  (notmuch-show-next-thread-show))

;;;###autoload
(defun +notmuch/search-spam ()
  (interactive)
  (notmuch-search-add-tag +notmuch-spam-tags)
  (notmuch-search-next-thread))

;;;###autoload
(defun +notmuch/tree-spam ()
  (interactive)
  (notmuch-tree-add-tag +notmuch-spam-tags)
  (notmuch-tree-next-message))

;;;###autoload
(defun +notmuch/compose ()
  "Compose new mail"
  (interactive)
  (notmuch-mua-mail
   nil
   nil
   (list (cons 'From  (completing-read "From: " (notmuch-user-emails))))))

;;;###autoload
(defun +notmuch/open-message-with-mail-app-notmuch-tree ()
  (interactive)
  (let* ((msg-path (car (plist-get (notmuch-tree-get-message-properties) :filename)))
         (temp (make-temp-file "notmuch-message-" nil ".eml")))
    (doom-call-process "cp" msg-path temp)
    (start-process-shell-command "email" nil (format "xdg-open '%s'" temp))))

;;;###autoload
(defun +notmuch/open-message-with-mail-app-notmuch-show ()
  (interactive)
  (let* ((msg-path (car (plist-get (notmuch-show-get-message-properties) :filename)))
         (temp (make-temp-file "notmuch-message-" nil ".eml")))
    (doom-call-process "cp" msg-path temp)
    (start-process-shell-command "email" nil (format "xdg-open '%s'" temp))))


;;;###autoload
(defun +notmuch/show-filter-thread ()
  "Show the current thread with a different filter"
  (interactive)
  (setq notmuch-show-query-context (notmuch-read-query "Filter thread: "))
  (notmuch-show-refresh-view t))

;;;###autoload
(defun +notmuch-show-expand-only-unread-h ()
  (interactive)
  (let ((unread nil)
        (open (notmuch-show-get-message-ids-for-open-messages)))
    (notmuch-show-mapc (lambda ()
                         (when (member "unread" (notmuch-show-get-tags))
                           (setq unread t))))
    (when unread
      (let ((notmuch-show-hook (remove '+notmuch-show-expand-only-unread-h notmuch-show-hook)))
        (notmuch-show-filter-thread "tag:unread")))))

;;
;; Advice

;;;###autoload
(defun +notmuch-dont-confirm-on-kill-process-a (fn &rest args)
  "Don't prompt for confirmation when killing notmuch sentinel."
  (let (confirm-kill-processes)
    (apply fn args)))
