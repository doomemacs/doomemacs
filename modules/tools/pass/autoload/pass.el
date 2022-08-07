;;; tools/pass/autoload/pass.el -*- lexical-binding: t; -*-

(defun +pass--copy-username (entry)
  (if-let* ((user (+pass-get-field entry +pass-user-fields)))
      (progn (password-store-clear)
             (message "Copied username to the kill ring.")
             (kill-new user))
    (error "Username not found.")))


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
      (cl-loop for key in (ensure-list fields)
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
(define-obsolete-function-alias '+pass/edit-entry #'password-store-edit "21.12")

;;;###autoload
(define-obsolete-function-alias '+pass/copy-field #'password-store-copy-field "21.12")

;;;###autoload
(define-obsolete-function-alias '+pass/copy-secret #'password-store-copy "21.12")

;;;###autoload
(defun +pass/copy-user (entry)
  "Interactively search for an entry and copy the login to your clipboard. The
fields in `+pass-user-fields' is used to find the login field."
  (interactive
   (list (password-store--completing-read)))
  (+pass--copy-username entry))

;;;###autoload
(define-obsolete-function-alias '+pass/browse-url #'password-store-url "21.12")
