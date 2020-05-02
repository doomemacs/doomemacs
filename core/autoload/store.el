;;; core/autoload/cache.el -*- lexical-binding: t; -*-

;; This little library abstracts the process of writing arbitrary elisp values
;; to a 2-tiered file store (in `doom-store-dir'/`doom-store-location').

(defvar doom-store-dir (concat doom-cache-dir "store/")
  "Directory to look for and store data accessed through this API.")

(defvar doom-store-persist-alist '(t)
  "An alist of alists, containing lists of variables for the doom cache library
to persist across Emacs sessions.")

(defvar doom-store-location "default"
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`doom-store-dir'). One file may contain multiple cache entries.")

(defun doom-save-persistent-store-h ()
  "Hook to run when an Emacs session is killed. Saves all persisted variables
listed in `doom-store-persist-alist' to files."
  (dolist (alist (butlast doom-store-persist-alist 1))
    (cl-loop with key = (car alist)
             for var in (cdr alist)
             if (symbol-value var)
             do (doom-store-set var it nil key))))
(add-hook 'kill-emacs-hook #'doom-save-persistent-store-h)


;;
;; Library

;;;###autoload
(defun doom-store-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).

This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits.

Warning: this is incompatible with buffer-local variables."
  (dolist (var variables)
    (when (doom-store-exists var location)
      (set var (doom-store-get var location))))
  (setf (alist-get location doom-store-persist-alist)
        (append variables (cdr (assq location doom-store-persist-alist)))))

;;;###autoload
(defun doom-store-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol) from
`doom-store-persist-alist', thus preventing them from being saved between sessions.
Does not affect the actual variables themselves or their values."
  (if variables
      (setf (alist-get location doom-store-persist-alist)
            (cl-set-difference (cdr (assq location doom-store-persist-alist))
                               variables))
    (delq (assq location doom-store-persist-alist)
          doom-store-persist-alist)))

(defun doom--store-init (location)
  (or (gethash location doom--cache)
      (not (file-exists-p doom-store-dir))
      (let* ((store (expand-file-name location doom-store-dir))
             (data (and (file-exists-p store)
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (setq buffer-file-coding-system 'binary)
                          (let (file-name-handler-alist)
                            (insert-file-contents-literally store))
                          (setq data (read (current-buffer)))))))
        (puthash location data doom--cache)
        data)))

(defun doom--store-get (key location &optional ttl)
  (when-let* ((location-data (doom--store-init location))
              (data (gethash location location-data)))
    (and (or (null (car data))
             (time-less-p (time-add (current-time) ttl) (car data)))
         (cdr data))))

(defun doom--store-put (key value location &optional ttl)
  (let ((data (doom--store-init location)))
    (puthash location
             (cons (time-add (current-time) ttl)
                   (doom--store-get key location))
             data)
    (let ((coding-system-for-write 'binary)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (with-temp-file (expand-file-name location doom-store-dir)
        (prin1 data (current-buffer))))
    data))


(defvar doom--cache (make-hash-table :test 'equal))
;;;###autoload
(defun doom-store-get (key &optional location)
  "Retrieve KEY from LOCATION (defaults to `doom-store-location'), if it exists
and hasn't expired."
  (doom--store-get key (or location doom-store-location)))

;;;###autoload
(defun doom-store-set (key value &optional ttl location)
  "Set KEY to VALUE in the cache. TTL is the time (in seconds) until this cache
entry expires. LOCATION is the super-key to store this cache item under; the
default is `doom-store-location'. "
  (doom--store-put key value (or location doom-store-location) ttl))

;;;###autoload
(defalias 'doom-store-exists #'doom-store-get)

;;;###autoload
(defun doom-store-clear (&optional location)
  "Clear a cache LOCATION (defaults to `doom-store-location')."
  (let ((path (expand-file-name (or location doom-store-location) doom-store-location)))
    (when (file-exists-p path)
      (delete-file path)
      t)))
