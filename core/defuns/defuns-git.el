;;; defuns-git.el

;;;###autoload (autoload 'doom:git-remote-browse "defuns-git" nil t)
(evil-define-command doom:git-remote-browse (&optional bang)
  "Open the website for the current (or specified) version controlled FILE. If
BANG, then use hub to do it."
  (interactive "<!>")
  (let (url)
    (condition-case err
        (setq url (browse-at-remote/get-url))
      (error
       (setq url (shell-command-to-string "hub browse -u --"))
       (setq url (if url
                     (concat (s-trim url) "/" (f-relative (buffer-file-name) (doom/project-root))
                             (when (use-region-p) (format "#L%s-L%s"
                                                          (line-number-at-pos (region-beginning))
                                                          (line-number-at-pos (region-end)))))))))
    (when url
      (if bang
          (message "Url copied to clipboard: %s" (kill-new url))
        (browse-url url)))))

(provide 'defuns-git)
;;; defuns-git.el ends here
