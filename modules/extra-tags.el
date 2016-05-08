;;; extra-tags.el

;; WIP

(defvar narf-ctags-alist
  '((ruby-mode :exec "ripper-tags -R -f %t --emacs")
    (c++-mode :ext "cpp")
    (c-mode :ext "c")
    (all :exec "find . -type f -iname \"*.%e\" | ctags -e -f %t -")
    ;; TODO add c-mode/c++-mode ctags
    ))


(add-hook 'find-file-hook 'narf|init-tags)
(defun narf|init-tags ()
  (awhen (narf/tags-p)
    (setq tags-table-list (list it))))

;; TODO
;; (defun narf/tags-generate ()
;;   (interactive)
;;   (let ((command (assoc major-mode narf-ctags-alist))
;;         (default-directory (narf/project-root)))
;;     (unless command
;;       (user-error "No tag generator for %s" major-mode))
;;     (async-shell-command command "*narf:generate-tags*")))

;;;###autoload
(defun narf/find-def ()
  (interactive)
  (let ((orig-pt (point))
        (orig-file (buffer-file-name)))
    (cond ((and (narf/tags-p)
                (progn (call-interactively 'helm-etags-select)
                       (and (/= orig-pt (point))
                            (f-same? orig-file buffer-file-name)))))
          ((progn (dumb-jump-go)
                  (and (/= orig-pt (point))
                       (f-same? orig-file buffer-file-name))))
          (t (call-interactively 'evil-goto-definition)))))

;;;###autoload
(defun narf/tags-p ()
  (let ((path (expand-file-name ".tags" (narf/project-root))))
    (and (f-exists? path) path)))

(provide 'extra-tags)
;;; extra-tags.el ends here
