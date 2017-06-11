;;; tools/password-store/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +pass/open ()
  (interactive)
  (cond ((featurep! :completion ivy)
         (+pass-ivy))
        ((featurep! :completion helm)
         (helm-pass))
        (t
         (pass))))


(after! ivy
  (defun +pass-ivy-action--get-field (item)
    (let* ((data (auth-pass-parse-entry item))
           (field (if data (completing-read "Field: " (mapcar #'car data) nil t))))
      (if data
          (progn
            (password-store-clear)
            (message "Copied %s's %s field to clipboard. Will clear in %s seconds"
                     item field (password-store-timeout))
            (kill-new (cdr (assoc field data)))
            (setq password-store-timeout-timer
                  (run-at-time (password-store-timeout) nil 'password-store-clear)))
        (error "Couldn't find entry: %s" item))))

  (defun +pass-ivy-action--copy-username (item)
    (if-let (user (cl-loop with data = (auth-pass-parse-entry item)
                           for key in +pass-user-fields
                           when (assoc key data)
                           return (cdr it)))
        (progn (password-store-clear)
               (message "Copied username to the kill ring.")
               (kill-new user))
      (message "Username not found!")))

  (ivy-add-actions
   '+pass/ivy
   '(("o" password-store-copy "copy password")
     ("u" +pass-ivy-action--copy-username "copy username")
     ("e" password-store-edit "edit entry")
     ("b" password-store-url "open url in browser")
     ("f" +pass-ivy-action--get-field "get field"))))

;;;###autoload
(defun +pass/ivy (&optional browse-url)
  (interactive "P")
  (ivy-read "Pass: " (password-store-list)
            :action (if browse-url
                        #'password-store-url
                      #'password-store-copy)
            :caller '+pass/ivy))

