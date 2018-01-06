;;; lang/org/init.el -*- lexical-binding: t; -*-

;; Ensure ELPA org is prioritized above built-in org.
(eval-and-compile
  (when-let* ((path (locate-library "org" nil doom--base-load-path)))
    (setq load-path
          (delete (substring (file-name-directory path) 0 -1)
                  load-path))))

