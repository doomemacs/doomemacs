;;; lang/org/init.el -*- lexical-binding: t; -*-

;; Ensure ELPA org is prioritized above built-in org.
(eval-and-compile
  (when-let* ((old-path (locate-library "org" nil doom--base-load-path)))
    (setq old-path (substring (file-name-directory old-path) 0 -1))
    (delete old-path load-path)

    ;; We remove it from the base load path too so that `doom//reload-load-path'
    ;; won't undo this modification.
    (delete old-path doom--base-load-path)))
