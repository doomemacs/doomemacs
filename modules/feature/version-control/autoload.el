;;; feature/version-control/autoload.el

;;;###autoload
(defun +vcs-root ()
  "Get git url root."
  (when-let (url (car-safe (browse-at-remote--remote-ref buffer-file-name)))
    (cdr (browse-at-remote--get-url-from-remote url))))

;;;###autoload
(defun +vcs/git-browse ()
  "Open the website for the current version controlled file. Fallback to
repository root."
  (interactive)
  (let (url)
    (condition-case err
        (setq url (browse-at-remote-get-url))
      ('error
       (setq url (shell-command-to-string "hub browse -u --"))
       (setq url (if url
                     (concat (string-trim url) "/blob/"
                             (or (car (vc-git-branches)) "master") "/"
                             (file-relative-name (file-truename (buffer-file-name))
                                                 (file-truename (doom-project-root)))
                             (when (use-region-p)
                               (format "#L%s-L%s"
                                       (line-number-at-pos (region-beginning))
                                       (line-number-at-pos (region-end)))))))))
    (when url (browse-url url))))

;;;###autoload
(defun +vcs/git-browse-issues ()
  "Open the github issues page for current repo."
  (interactive)
  (if-let (root (+vcs-root))
      (browse-url (concat root "/issues"))
    (user-error "No git root found!")))
