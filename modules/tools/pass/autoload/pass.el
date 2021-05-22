;;; tools/pass/autoload/pass.el -*- lexical-binding: t; -*-

(defun +pass--open-url (entry)
  (if-let* ((url (+pass-get-field entry +pass-url-fields)))
      (and (or (string-match-p "https?://" url)
               (error "Field for %s doesn't look like an url" item))
           (browse-url url))
    (error "url not found.")))

(defun +pass--copy (entry field text)
  (password-store-clear)
  (message "Copied %s's %s field to clipboard. Will clear in %s seconds"
           entry field (password-store-timeout))
  (kill-new text)
  (setq password-store-timeout-timer
        (run-at-time (password-store-timeout) nil 'password-store-clear)))

(defun +pass--copy-username (entry)
  (if-let* ((user (+pass-get-field entry +pass-user-fields)))
      (progn (password-store-clear)
             (message "Copied username to the kill ring.")
             (kill-new user))
    (error "Username not found.")))

(defun +pass--completing-read-field (entry)
  (let* ((data (+pass-get-entry entry))
         (field (if data (completing-read "Field: " (mapcar #'car data) nil t))))
    (+pass-get-field data field)))


;;
;; API

;;;###autoload (autoload 'auth-source-pass-parse-entry "auth-source-pass")
;;;###autoload
(defalias '+pass-get-entry #'auth-source-pass-parse-entry)

;;;###autoload
(defun +pass-get-field (entry fields &optional noerror)
  "Fetches the value of a field. FIELDS can be a list of string field names or a
single one. If a list, the first field found will be returned. Will error out
otherwise, unless NOERROR is non-nill."
  (if-let* ((data (if (listp entry) entry (+pass-get-entry entry))))
      (cl-loop for key in (doom-enlist fields)
               when (assoc key data)
               return (cdr it))
    (unless noerror
      (error "Couldn't find entry: %s" entry))))

;;;###autoload
(defun +pass-get-user (entry)
  "Fetches the user field from ENTRY. Each of `+pass-user-fields' are tried in
search of your username. May prompt for your gpg passphrase."
  (+pass-get-field entry +pass-user-fields))

;;;###autoload
(defun +pass-get-secret (entry)
  "Fetches your secret from ENTRY. May prompt for your gpg passphrase."
  (+pass-get-field entry 'secret))


;;
;; Commands

;;;###autoload (autoload 'password-store-dir "password-store")
;;;###autoload (autoload 'password-store-list "password-store")
;;;###autoload (autoload 'password-store--completing-read "password-store")

;;;###autoload
(defun +pass/edit-entry (entry)
  "Interactively search for and open a pass entry for editing."
  (interactive
   (list (password-store--completing-read)))
  (find-file (concat (expand-file-name entry (password-store-dir)) ".gpg")))

;;;###autoload
(defun +pass/copy-field (entry)
  "Interactively search for an entry and copy a particular field to your
clipboard."
  (interactive
   (list (password-store--completing-read)))
  (let* ((data (+pass-get-entry entry))
         (field (if data (completing-read "Field: " (mapcar #'car data) nil t))))
    (+pass--copy entry field (+pass-get-field data field))))

;;;###autoload
(defun +pass/copy-secret (entry)
  "Interactively search for an entry and copy its secret/password to your
clipboard."
  (interactive
   (list (password-store--completing-read)))
  (+pass--copy entry 'secret (+pass-get-secret entry)))

;;;###autoload
(defun +pass/copy-user (entry)
  "Interactively search for an entry and copy the login to your clipboard. The
fields in `+pass-user-fields' is used to find the login field."
  (interactive
   (list (password-store--completing-read)))
  (+pass--copy-username entry))

;;;###autoload
(defun +pass/browse-url (entry)
  "Interactively search for an entry and open its url in your browser. The
fields in `+pass-url-fields' is used to find the url field."
  (interactive
   (list (password-store--completing-read)))
  (+pass--open-url entry))
