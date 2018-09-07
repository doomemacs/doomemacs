;;; ../core/autoload/cache.el -*- lexical-binding: t; -*-

;; This little library thinly wraps around persistent-soft (which is a pcache
;; wrapper, how about that). It has three purposes:
;;
;; + To encapsulate the cache backend (persistent-soft/pcache in this case), in
;;   case it needs to change.
;; + To provide `doom-cache-persist': a mechanism for easily persisting
;;   variables across Emacs sessions.
;; + To lazy-load persistent-soft until it is really needed.
;;
;; Like persistent-soft, caches assume a 2-tier structure, where all caches are
;; namespaced by location.

(defvar doom-cache-alists '(t)
  "An alist of alists, containing lists of variables for the doom cache library
to persist across Emacs sessions.")

(defvar doom-cache-location 'doom
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`doom-cache-dir'). One file may contain multiple cache entries.")

(defun doom|save-persistent-cache ()
  "Hook to run when an Emacs session is killed. Saves all persisted variables
listed in `doom-cache-alists' to files."
  (dolist (alist (butlast doom-cache-alists 1))
    (cl-loop with key = (car alist)
             for var in (cdr alist)
             if (symbol-value var)
             do (doom-cache-set var it nil key))))
(add-hook 'kill-emacs-hook #'doom|save-persistent-cache)


;;
;; Library

;;;###autoload
(defmacro with-cache! (location &rest body)
  "Runs BODY with a different default `doom-cache-location'."
  (declare (indent defun))
  `(let ((doom-cache-location ',location))
     ,@body))

;;;###autoload
(defun doom-cache-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).

This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits.

Warning: this is incompatible with buffer-local variables."
  (dolist (var variables)
    (when (doom-cache-exists var location)
      (set var (doom-cache-get var location))))
  (setf (alist-get location doom-cache-alists)
        (append variables (cdr (assq location doom-cache-alists)))))

;;;###autoload
(defun doom-cache-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol) from
`doom-cache-alists', thus preventing them from being saved between sessions.
Does not affect the actual variables themselves or their values."
  (if variables
      (setf (alist-get location doom-cache-alists)
            (cl-set-difference (cdr (assq location doom-cache-alists))
                               variables))
    (delq (assq location doom-cache-alists)
          doom-cache-alists)))

;;;###autoload
(defun doom-cache-get (key &optional location)
  "Retrieve KEY from LOCATION (defaults to `doom-cache-location'), if it exists
and hasn't expired."
  (persistent-soft-fetch
   key (symbol-name (or location doom-cache-location))))

;;;###autoload
(defun doom-cache-set (key value &optional ttl location)
  "Set KEY to VALUE in the cache. TTL is the time (in seconds) until this cache
entry expires. LOCATION is the super-key to store this cache item under; the
default is `doom-cache-location'. "
  (persistent-soft-store
   key value
   (symbol-name (or location doom-cache-location)) ttl))

;;;###autoload
(defun doom-cache-exists (key &optional location)
  "Returns t if KEY exists at LOCATION (defaults to `doom-cache-location')."
  (persistent-soft-exists-p key (or location doom-cache-location)))

;;;###autoload
(defun doom-cache-clear (&optional location)
  "Clear a cache LOCATION (defaults to `doom-cache-location')."
  (persistent-soft-flush (or location doom-cache-location)))
