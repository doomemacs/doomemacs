;;; emacs/org/+export.el --- -*- no-byte-compile: t; -*-

;; My own, centralized exporting system as well.

(add-hook '+org-init-hook '+org|init-export t)

(defun +org|init-export ()
  (setq org-export-directory (expand-file-name ".export" +org-dir)
        org-export-backends '(ascii html latex md)
        org-export-with-toc t
        org-export-with-author t)

  ;; Export to a central directory (why isn't this easier?)
  (unless (file-directory-p org-export-directory)
    (make-directory org-export-directory t))
  (defun +org*export-output-file-name (args)
    (unless (nth 2 args)
      (setq args (append args (list org-export-directory))))
    args)
  (advice-add 'org-export-output-file-name :filter-args '+org*export-output-file-name)

  ;; (require 'ox-pandoc)
  ;; (setq org-pandoc-options '((standalone . t) (mathjax . t) (parse-raw . t)))

  ;; keybinds
  ;; (@map :leader :n "oe" (@find-file-in org-export-directory))
  )

