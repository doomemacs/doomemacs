;;; lisp/cli/gc.el --- clean up after profiles, packages, and logs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(doom-require 'doom-lib 'packages)


;;
;;; Variables

;; None yet!


;;
;;; Helpers

(defun doom-gc--build (build)
  (let ((build-dir (straight--build-dir build)))
    (delete-directory build-dir 'recursive)
    (if (file-directory-p build-dir)
        (ignore (print! (error "Failed to purg build/%s" build)))
      (print! (success "Purged build/%s" build))
      t)))

(defun doom-gc--builds (builds)
  (if (not builds)
      (prog1 0
        (print! (item "No builds to purge")))
    (print! (start "Purging straight builds..." (length builds)))
    (print-group!
      (length
       (delq nil (mapcar #'doom-gc--build builds))))))

(defun doom-gc--elpa ()
  (let ((dirs (doom-files-in package-user-dir :type t :depth 0)))
    (if (not dirs)
        (prog1 0
          (print! (item "No ELPA packages to purge")))
      (print! (start "Purging ELPA packages..."))
      (dolist (path dirs (length dirs))
        (condition-case e
            (print-group!
              (if (file-directory-p path)
                  (delete-directory path 'recursive)
                (delete-file path))
              (print! (success "Deleted %s") (filename path)))
          (error
           (print! (error "Failed to delete %s because: %s")
                   (filename path)
                   e)))))))

(defun doom-gc--repo (repo)
  (let ((repo-dir (straight--repos-dir repo)))
    (when (file-directory-p repo-dir)
      (delete-directory repo-dir 'recursive)
      (delete-file (straight--modified-file repo))
      (if (file-directory-p repo-dir)
          (ignore (print! (error "Failed to purge repos/%s" repo)))
        (print! (success "Purged repos/%s" repo))
        t))))

(defun doom-gc--repos (repos)
  (if (not repos)
      (prog1 0
        (print! (item "No repos to purge")))
    (print! (start "Purging straight repositories..."))
    (print-group!
      (length
       (delq nil (mapcar #'doom-gc--repo repos))))))

(defun doom-gc--eln ()
  (if-let* ((dirs
             (cl-delete (expand-file-name comp-native-version-dir doom-packages--eln-output-path)
                        (directory-files doom-packages--eln-output-path t "^[^.]" t)
                        :test #'file-equal-p)))
      (progn
        (print! (start "Purging old native bytecode..."))
        (print-group!
          (dolist (dir dirs)
            (print! (item "Deleting %S") (relpath dir doom-packages--eln-output-path))
            (delete-directory dir 'recursive))
          (print! (success "Purged %d directory(ies)" (length dirs))))
        (length dirs))
    (print! (item "No ELN directories to purge"))
    0))

(cl-defun doom-gc--regraft-repo (repo)
  (unless repo
    (error "No repo specified for regrafting"))
  (let ((default-directory (straight--repos-dir repo)))
    (unless (file-directory-p ".git")
      (print! (warn "\rrepos/%s is not a git repo, skipping" repo))
      (cl-return))
    (unless (file-in-directory-p default-directory straight-base-dir)
      (print! (warn "\rSkipping repos/%s because it is local" repo))
      (cl-return))
    (let ((before-size (doom-directory-size default-directory)))
      (doom-call-process "git" "reset" "--hard")
      (doom-call-process "git" "clean" "-ffd")
      (if (not (zerop (car (doom-call-process "git" "replace" "--graft" "HEAD"))))
          (print! (item "\rrepos/%s is already compact\033[1A" repo))
        (doom-call-process "git" "reflog" "expire" "--expire=all" "--all")
        (doom-call-process "git" "gc" "--prune=now")
        (let ((after-size (doom-directory-size default-directory)))
          (if (equal after-size before-size)
              (print! (success "\rrepos/%s cannot be compacted further" repo))
            (print! (success "\rRegrafted repos/%s (from %0.1fKB to %0.1fKB)")
                    repo before-size after-size)))))
    t))

(defun doom-gc--regraft-repos (repos)
  (if (not repos)
      (prog1 0
        (print! (item "No repos to regraft")))
    (print! (start "Regrafting %d repos..." (length repos)))
    (let ((before-size (doom-directory-size (straight--repos-dir))))
      (print-group!
        (prog1 (delq nil (mapcar #'doom-gc--regraft-repo repos))
          ;; (princ "\r\033[K")
          (let ((after-size (doom-directory-size (straight--repos-dir))))
            (print! (success "\rFinished regrafting. Size before: %0.1fKB and after: %0.1fKB (%0.1fKB)")
                    before-size after-size
                    (- after-size before-size))))))))

;;
;;; Commands

(defcli! (gc)
    ((nobuilds-p  ("-b" "--no-builds")  "Don't purge unneeded (built) packages")
     (noelpa-p    ("-p" "--no-elpa")    "Don't purge ELPA packages")
     (norepos-p   ("-r" "--no-repos")   "Don't purge unused straight repos")
     (noeln-p     ("-e" "--no-eln")     "Don't purge old ELN bytecode")
     (noregraft-p ("-g" "--no-regraft") "Don't regraft git repos (ie. compact them)"))
  "Deletes orphaned packages & repos, and compacts them.

Purges all installed ELPA packages (as they are considered temporary). Purges
all orphaned package repos and builds. Also regrafts and compacts package repos
to ensure they are as small as possible.

It is a good idea to occasionally run this command to ensure your package list
remains lean."
  :benchmark t
  :group 'emacs
  (require 'comp nil t)
  (doom-initialize-packages)
  (doom-packages--barf-if-incomplete)
  (print! (start "Purging orphaned packages (for the emperor)..."))
  (quiet! (straight-prune-build-cache))
  (cl-destructuring-bind (&optional builds-to-purge repos-to-purge repos-to-regraft)
      (let ((rdirs
             (when (or (not norepos-p) (not noregraft-p))
               (straight--directory-files (straight--repos-dir) nil nil 'sort))))
        (list (unless nobuilds-p
                (let ((default-directory (straight--build-dir)))
                  (seq-filter #'file-directory-p
                              (seq-remove (doom-rpartial #'gethash straight--profile-cache)
                                          (straight--directory-files default-directory nil nil 'sort)))))
              (unless norepos-p
                (seq-remove (doom-rpartial #'straight--checkhash straight--repo-cache)
                            rdirs))
              (unless noregraft-p
                (seq-filter (doom-rpartial #'straight--checkhash straight--repo-cache)
                            rdirs))))
    (print-group!
      (delq
       nil (list
            (if nobuilds-p
                (ignore (print! (item "Skipping builds")))
              (/= 0 (doom-gc--builds builds-to-purge)))
            (if noelpa-p
                (ignore (print! (item "Skipping elpa packages")))
              (/= 0 (doom-gc--elpa)))
            (if norepos-p
                (ignore (print! (item "Skipping repos")))
              (/= 0 (doom-gc--repos repos-to-purge)))
            (if noregraft-p
                (ignore (print! (item "Skipping regrafting")))
              (doom-gc--regraft-repos repos-to-regraft))
            (when (featurep 'native-compile)
              (if noeln-p
                  (ignore (print! (item "Skipping native bytecode")))
                (doom-gc--eln)))))))
  t)

;; (defcli! gc
;;     ((keep           (     "--keep" time)     "Don't delete generations in the last TIME units")
;;      (keep-last      (     "--keep-last" num) "Don't delete last NUM generations")
;;      (keep-elpa-p    ("-e" "--keep-elpa")     "Don't delete package.el-installed packages")
;;      (keep-orphans-p ("-o" "--keep-orphaned") "Don't delete unused packages")
;;      (keep-history-p (     "--keep-history")  "Don't regraft repos"))
;;   "Purge unused profile data, generations, and packages.")

(provide 'doom-cli-gc)
;;; gc.el ends here
