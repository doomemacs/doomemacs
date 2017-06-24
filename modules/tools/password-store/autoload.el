;;; tools/password-store/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +pass/open ()
  (interactive)
  (cond ((featurep! :completion ivy)
         (+pass/ivy))
        ((featurep! :completion helm)
         (helm-pass))
        (t
         (pass))))

;;;###autoload
(defun +pass-get-field (entry fields)
  (if-let (data (if (listp entry) entry (auth-pass-parse-entry entry)))
      (cl-loop for key in (doom-enlist fields)
               when (assoc key data)
               return (cdr it))
    (error "Couldn't find entry: %s" entry)))

;;;###autoload
(defun +pass-get-user (entry)
  (+pass-get-field entry +pass-user-fields))

;;;###autoload
(defun +pass-get-secret (entry)
  (+pass-get-field entry 'secret))

(defun +pass-ivy-action--open-url (entry)
  (if-let (url (+pass-get-field entry +pass-url-fields))
      (and (or (string-match-p "https?://" url)
               (error "Field for %s doesn't look like an url" item))
           (browse-url url))
    (error "Username not found.")))

(defun +pass-ivy-action--get-field (item)
  (let* ((data (auth-pass-parse-entry item))
          (field (if data (completing-read "Field: " (mapcar #'car data) nil t))))
    (if data
        (progn
          (password-store-clear)
          (message "Copied %s's %s field to clipboard. Will clear in %s seconds"
                    item field (password-store-timeout))
          (kill-new (+pass-get-field data field))
          (setq password-store-timeout-timer
                (run-at-time (password-store-timeout) nil 'password-store-clear)))
      (error "Couldn't find entry: %s" item))))

(defun +pass-ivy-action--copy-username (entry)
  (if-let (user (+pass-get-field entry +pass-user-fields))
      (progn (password-store-clear)
             (message "Copied username to the kill ring.")
             (kill-new user))
    (error "Username not found.")))

(after! ivy
  (ivy-add-actions
   '+pass/ivy
   '(("o" password-store-copy "copy password")
     ("e" password-store-edit "edit entry")
     ("u" +pass-ivy-action--copy-username "copy username")
     ("b" +pass-ivy-action--open-url "open url in browser")
     ("f" +pass-ivy-action--get-field "get field"))))

;;;###autoload
(defun +pass/ivy (&optional browse-url)
  (interactive "P")
  (ivy-read "Pass: " (password-store-list)
            :action (if browse-url
                        #'password-store-url
                      #'password-store-copy)
            :caller '+pass/ivy))

