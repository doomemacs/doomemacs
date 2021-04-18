;;; core/autoload/cache.el -*- lexical-binding: t; -*-

;; This little library abstracts the process of writing arbitrary elisp values
;; to a 2-tiered file store (in `doom-store-dir'/`doom-store-location').

(defvar doom-store-dir (concat doom-etc-dir "store/")
  "Directory to look for and store data accessed through this API.")

(defvar doom-store-persist-alist ()
  "An alist of alists, containing lists of variables for the doom cache library
to persist across Emacs sessions.")

(defvar doom-store-location "default"
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`doom-store-dir'). One file may contain multiple cache entries.")

(defvar doom--store-table (make-hash-table :test 'equal))

(defun doom-save-persistent-store-h ()
  "Hook to persist `doom-store's storage when Emacs is killed."
  (let (locations)
    ;; Persist `doom-store-persist-alist'
    (dolist (alist (butlast doom-store-persist-alist 1))
      (cl-loop with location = (car alist)
               for var in (cdr alist)
               do (doom-store-put var (symbol-value var) nil location 'noflush)
               and do (cl-pushnew location locations :test #'equal)))
    ;; Clean up expired entries,
    (dolist (location (doom-files-in doom-store-dir :relative-to doom-store-dir))
      (maphash (lambda (key val)
                 (when (doom--store-expired-p key val)
                   (cl-pushnew location locations :test #'equal)
                   (doom--store-rem key location 'noflush)))
               (doom--store-init location)))
    (mapc #'doom--store-flush locations)))
(add-hook 'kill-emacs-hook #'doom-save-persistent-store-h)


;;
;;; Library

;;;###autoload
(defun doom-store-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).
This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits. This cannot persist buffer-local variables."
  (cl-check-type location string)
  (dolist (var variables)
    (when (doom-store-member-p var location)
      (set var (doom-store-get var location))))
  (setf (alist-get location doom-store-persist-alist)
        (append variables (alist-get location doom-store-persist-alist))))

;;;###autoload
(defun doom-store-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol).
Variables to persist are recorded in `doom-store-persist-alist'. Does not affect
the actual variables themselves or their values."
  (cl-check-type location string)
  (if variables
      (setf (alist-get location doom-store-persist-alist)
            (cl-set-difference (cdr (assq location doom-store-persist-alist))
                               variables))
    (delq! location doom-store-persist-alist 'assoc)))

(defun doom--store-init (&optional location)
  (cl-check-type location (or null string))
  (let ((location (or location doom-store-location)))
    (or (gethash location doom--store-table)
        (let* ((file-name-handler-alist nil)
               (location-path (expand-file-name location doom-store-dir)))
          (if (file-exists-p location-path)
              (puthash location
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (setq buffer-file-coding-system 'binary)
                         (insert-file-contents-literally location-path)
                         (read (current-buffer)))
                       doom--store-table)
            (puthash location (make-hash-table :test 'equal)
                     doom--store-table))))))

(defun doom--store-expired-p (key data)
  (let ((ttl (car data)))
    (cond ((functionp ttl)
           (not (funcall ttl key data)))
          ((consp ttl)
           (time-less-p ttl (current-time))))))

(defun doom--store-flush (location)
  "Write `doom--store-table' to `doom-store-dir'."
  (let ((file-name-handler-alist nil)
        (coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (let* ((location (or location doom-store-location))
           (data (doom--store-init location)))
      (make-directory doom-store-dir 'parents)
      (with-temp-file (expand-file-name location doom-store-dir)
        (prin1 data (current-buffer)))
      data)))


;;;###autoload
(defun doom-store-get (key &optional location default-value noflush)
  "Retrieve KEY from LOCATION (defaults to `doom-store-location').
If it doesn't exist or has expired, DEFAULT_VALUE is returned."
  (let ((data (gethash key (doom--store-init location) default-value)))
    (if (not (or (eq data default-value)
                 (doom--store-expired-p key data)))
        (cdr data)
      (doom-store-rem key location noflush)
      default-value)))

;;;###autoload
(defun doom-store-put (key value &optional ttl location noflush)
  "Set KEY to VALUE in the store at LOCATION.
KEY can be any lisp object that is comparable with `equal'. TTL is the duration
(in seconds) after which this cache entry expires; if nil, no cache expiration.
LOCATION is the super-key to store this cache item under. It defaults to
`doom-store-location'."
  (cl-check-type ttl (or null integer function))
  (puthash key (cons (if (integerp ttl)
                         (time-add (current-time) ttl)
                       ttl)
                     value)
           (doom--store-init location))
  (unless noflush
    (doom--store-flush location)))

;;;###autoload
(defun doom-store-rem (key &optional location noflush)
  "Clear a cache LOCATION (defaults to `doom-store-location')."
  (remhash key (doom--store-init location))
  (unless noflush
    (doom--store-flush location)))

;;;###autoload
(defun doom-store-member-p (key &optional location)
  "Return t if KEY in LOCATION exists.
LOCATION defaults to `doom-store-location'."
  (let ((nil-value (format "--nilvalue%s--" (current-time))))
    (not (equal (doom-store-get key location nil-value)
                nil-value))))

;;;###autoload
(defun doom-store-clear (&optional location)
  "Clear the store at LOCATION (defaults to `doom-store-location')."
  (let* ((location (or location doom-store-location))
         (path (expand-file-name location doom-store-dir)))
    (remhash location doom--store-table)
    (when (file-exists-p path)
      (delete-file path)
      t)))
