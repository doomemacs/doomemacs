;;; core/autoload/cache.el -*- lexical-binding: t; -*-

;; This little library abstracts the process of writing arbitrary elisp values
;; to a 2-tiered file store (in `doom-store-dir'/`doom-store-location').

(defvar doom-store-dir (concat doom-etc-dir "store/")
  "Directory to look for and store data accessed through this API.")

(defvar doom-store-persist-alist '(t)
  "An alist of alists, containing lists of variables for the doom cache library
to persist across Emacs sessions.")

(defvar doom-store-location "default"
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`doom-store-dir'). One file may contain multiple cache entries.")

(defvar doom--store-table (make-hash-table :test 'equal))
(defvar doom--inhibit-flush nil)

(defun doom-save-persistent-store-h ()
  "Hook to run when an Emacs session is killed. Saves all persisted variables
listed in `doom-store-persist-alist' to files."
  (let (locations)
    (let ((doom--inhibit-flush t))
      (dolist (alist (butlast doom-store-persist-alist 1))
        (cl-loop with location = (car alist)
                 for var in (cdr alist)
                 do (doom-store-put var (symbol-value var) nil location)
                 and do (cl-pushnew location locations))))
    (mapc #'doom--store-flush locations)))
(add-hook 'kill-emacs-hook #'doom-save-persistent-store-h)


;;
;; Library

;;;###autoload
(defun doom-store-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).
This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits. This cannot persist buffer-local variables."
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
  (if variables
      (setf (alist-get location doom-store-persist-alist)
            (cl-set-difference (cdr (assq location doom-store-persist-alist))
                               variables))
    (delq! location doom-store-persist-alist 'assoc)))

(defun doom--store-init (location)
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
                   doom--store-table)))))

(defun doom--store-get (key location &optional default-value)
  (let* ((location-data (doom--store-init location))
         (data (gethash key location-data default-value)))
    (if (and (not (eq data default-value))
             (or (null (car data))
                 (not (time-less-p (car data) (current-time)))))
        (cdr data)
      default-value)))

(defun doom--store-put (key value location &optional ttl)
  (puthash key (cons (if ttl (time-add (current-time) ttl)) value)
           (doom--store-init location))
  (doom--store-flush location))

(defun doom--store-flush (location)
  (unless doom--inhibit-flush
    (let ((file-name-handler-alist nil)
          (coding-system-for-write 'binary)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil)
          (data (doom--store-init location)))
      (make-directory doom-store-dir 'parents)
      (with-temp-file (expand-file-name location doom-store-dir)
        (prin1 data (current-buffer)))
      data)))


;;;###autoload
(defun doom-store-get (key &optional location default-value)
  "Retrieve KEY from LOCATION (defaults to `doom-store-location').
If it doesn't exist or has expired, DEFAULT_VALUE is returned."
  (doom--store-get key (or location doom-store-location) default-value))

;;;###autoload
(defun doom-store-put (key value &optional ttl location)
  "Set KEY to VALUE in the store at LOCATION.
KEY can be any lisp object that is comparable with `equal'. TTL is the time (in
seconds) until this cache entry expires. LOCATION is the super-key to store this
cache item under. It defaults to `doom-store-location'."
  (doom--store-put key value (or location doom-store-location) ttl))

;;;###autoload
(defun doom-store-rem (key &optional location)
  "Clear a cache LOCATION (defaults to `doom-store-location')."
  (let ((location (or location doom-store-location)))
    (remhash key (doom--store-init location))
    (let ((table (doom--store-init "default")))
      (remhash 'test table)
      table)
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
