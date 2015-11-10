;;; macros-auto-insert.el
;; for ../core-auto-insert.el

;;;###autoload
(defmacro add-template! (regexp-or-major-mode uuid yas-mode &optional project-only)
  `(define-auto-insert ,(if (stringp regexp-or-major-mode)
                            regexp-or-major-mode
                          (eval regexp-or-major-mode))
     (lambda ()
       (unless (or (and ,project-only (not (narf/project-p)))
                   (not (or (eq major-mode ,yas-mode)
                            (and (boundp ,yas-mode)
                                 (symbol-value ,yas-mode)))))
         (insert ,uuid)
         (yas-expand-from-trigger-key)
         (if (string-equal ,uuid (s-trim (buffer-string)))
             (erase-buffer)
           (evil-insert-state 1))))))

(provide 'macros-auto-insert)
;;; macros-auto-insert.el ends here
