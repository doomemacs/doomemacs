;;; defuns-use-package.el

;; Meant to be loaded explicitly by core.el

(defun use-package--add-keyword (keyword after)
 (setq use-package-keywords
  (-insert-at (-find-index (lambda (key) (eq key after)) use-package-keywords)
   keyword use-package-keywords)))

(use-package--add-keyword :after :load-path)

(defalias 'use-package-normalize/:after 'use-package-normalize-symlist)

(defun use-package-handler/:after (name-symbol keyword arg rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (if (null arg)
        body
      (use-package-concat
       (use-package-process-keywords name-symbol
         (use-package-sort-keywords (use-package-plist-maybe-put rest :defer t)) state)
       (apply #'nconc
              (mapcar (lambda (feature)
                        `((after! ,feature (require ',name-symbol))))
                      (delete-dups arg)))))))

(provide 'defuns-use-package)
;;; defuns-use-package.el ends here
