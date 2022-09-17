;;; lisp/cli/profiles.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

(defvar doom-cli-known-profiles-file
  (file-name-concat doom-cache-dir (format "profiles.%s.el" (or (car doom-profile) "@")))
  ;; REVIEW: Use `doom-profile-data-dir' in v3
  ;; (file-name-concat doom-profile-data-dir "known-profiles.el")
  "TODO")


;;
;;; doom profile ...

(defcli-stub! ((profile pf)) ())

(defcli-stub! (profile hash) ())

(defcli-stub! (profile new) ())

(defcli-stub! (profile archive) ())

(defcli-stub! (profile (remove rm)) ())

(defcli-stub! (profile (rename mv)) ())

(defcli-stub! (profile nuke) ())


;;
;;; doom profiles ...

(defcli! (profiles sync) ((reload? ("--reload")))
  "Synchronize your profiles with Doom."
  :benchmark t
  (let* ((old-profiles (doom-profiles-read doom-cli-known-profiles-file))
         (new-profiles (doom-profiles-autodetect))
         (load-file doom-profile-load-file)
         (version (doom-file-read load-file :by 'read :noerror t))
         (recreate? (or (not reload?) (doom-profiles-outdated-p))))
    (unless (file-exists-p load-file)
      (print! (warn "No profile loader found. Generating one..."))
      (print-group! (print! (start "Regenerating it...")))
      (setq recreate? t))
    (unless (equal (or version doom-version) doom-version)
      (print! (warn "Detected version mismatch in profile loader (%s != %s)"
                    version doom-version))
      (print! (start "Generating profile manifest..."))
      (setq recreate? t))
    (print-group!
      (if (not recreate?)
          (doom-log "Profiles are up-to-date!")
        (let* ((pred    (lambda (a b) (eq (car a) (car b))))
               (added   (seq-difference new-profiles old-profiles pred))
               (removed (seq-difference old-profiles new-profiles pred))
               (changed (cl-loop for profile in (seq-intersection new-profiles old-profiles pred)
                                 unless (equal (cdr profile)
                                               (alist-get (car profile) old-profiles))
                                 collect profile)))
          (when (or added removed changed recreate?)
            (print! (start "Synchronizing %d known profile%s...")
                    (length new-profiles)
                    (if (/= (length new-profiles) 1) "s" ""))
            (print-group!
              (dolist (p added)   (print! (item "Added %S") (car p)))
              (dolist (p removed) (print! (item "Removed %S") (car p)))
              (dolist (p changed) (print! (item "Changed %S") (car p)))
              (doom-file-write doom-cli-known-profiles-file (list new-profiles) :mode #o600)
              (doom-profiles-save new-profiles load-file)
              (print! (success "Regenerated profile loader: %s")
                      (path load-file)))))))))


;;
;;; Helpers



(provide 'doom-cli-profiles)
;;; profiles.el ends here
