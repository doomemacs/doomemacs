;;; lang/org/+export.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-export)

;; I don't have any beef with org's built-in export system, but I do wish it
;; would export to a central directory (by default), rather than
;; `default-directory'. This is because all my org files are usually in one
;; place, and I want to be able to refer back to old exports if needed.

(defvar +org-export-dir ".export/"
  "Where to store exported files relative to `org-directory'. Can be an absolute
path too.")
(define-obsolete-variable-alias 'org-export-directory '+org-export-dir "2.1.0")


;;
(defun +org|init-export ()
  (setq org-export-backends '(ascii html latex md)
        org-publish-timestamp-directory (concat doom-cache-dir "org-timestamps/"))

  (when (and (executable-find "pandoc")
             (require 'ox-pandoc nil t))
    (add-to-list 'org-export-backends 'pandoc nil #'eq)
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://cdn.jsdelivr.net/npm/reveal.js@3/"))))

  ;; Export to a central location by default or if target isn't in
  ;; `org-directory'.
  (defun +org*export-output-file-name (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `org-directory'."
    (when (and (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p buffer-file-name org-directory))
      (cl-destructuring-bind (extension &optional subtreep _pubdir) args
        (let ((dir (expand-file-name +org-export-dir org-directory)))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (setq args (list extension subtreep dir)))))
    args)
  (advice-add #'org-export-output-file-name :filter-args #'+org*export-output-file-name))
