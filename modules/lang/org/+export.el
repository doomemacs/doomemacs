;;; lang/org/+export.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-export)

;; I don't have any beef with org's built-in export system, but I do wish it
;; would export to a central directory (by default), rather than
;; `default-directory'. This is because all my org files are usually in one
;; place, and I want to be able to refer back to old exports if needed.

(def-package! ox-pandoc
  :defer t
  :config
  (push 'pandoc org-export-backends)
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (parse-raw . t))))

;;
(defun +org|init-export ()
  (setq org-export-backends '(ascii html latex md)
        org-publish-timestamp-directory (concat doom-cache-dir "/org-timestamps/"))

  (when (executable-find "pandoc")
    (require 'ox-pandoc))

  ;; Export to a central location by default or if target isn't in `+org-dir'.
  (setq org-export-directory (expand-file-name ".export" +org-dir))
  (unless (file-directory-p org-export-directory)
    (make-directory org-export-directory t))

  (defun +org*export-output-file-name (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `+org-dir'."
    (when (and (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p (file-truename buffer-file-name) (file-truename +org-dir)))
      (setq args (append args (list org-export-directory))))
    args)
  (advice-add #'org-export-output-file-name :filter-args #'+org*export-output-file-name))
