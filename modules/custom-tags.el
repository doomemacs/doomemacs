;;; custom-tags.el

;; TODO Finish me!

(defvar doom-ctags-alist
  '((ruby-mode :exec "ripper-tags -R -f %t --emacs")
    (c++-mode :ext "cpp")
    (c-mode :ext "c")
    (all :exec "find . -type f -iname \"*.%e\" | ctags -e -f %t -")
    ;; TODO add c-mode/c++-mode ctags
    ))


(add-hook 'find-file-hook 'doom|init-tags)
(defun doom|init-tags ()
  (awhen (doom/tags-p)
    (setq tags-table-list (list it))))

;; TODO
;; (defun doom/tags-generate ()
;;   (interactive)
;;   (let ((command (assoc major-mode doom-ctags-alist))
;;         (default-directory (doom/project-root)))
;;     (unless command
;;       (user-error "No tag generator for %s" major-mode))
;;     (async-shell-command command "*doom:generate-tags*")))

;;;###autoload
(defun doom/find-def ()
  "Find definition using tags, falling back to dumb-jump otherwise."
  (interactive)
  (let ((orig-pt (point))
        (orig-file (buffer-file-name)))
    (cond ((and (doom/tags-p)
                ;; TODO
                ;; (progn (call-interactively 'helm-etags-select)
                ;;        (and (/= orig-pt (point))
                ;;             (f-same? orig-file buffer-file-name)))
                ))
          ((progn (dumb-jump-go)
                  (and (/= orig-pt (point))
                       (f-same? orig-file buffer-file-name))))
          (t (call-interactively 'evil-goto-definition)))))

;;;###autoload
(defun doom/tags-p ()
  (let ((path (expand-file-name ".tags" (doom/project-root))))
    (and (f-exists? path) path)))

(provide 'custom-tags)
;;; custom-tags.el ends here
