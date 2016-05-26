;;; core-docs.el

(use-package dash-at-point
  :when IS-MAC
  :commands (dash-at-point dash-at-point-with-docset dash-at-point-run-search
             dash-at-point-guess-docset)
  :init
  (defmacro def-docset! (mode docset)
    `(add-hook! ,mode (setq-local dash-at-point-docset ,docset)))
  (defun doom-docs-lookup (&optional search docset)
    (dash-at-point-run-search search docset)))

(use-package zeal-at-point
  :when (not IS-MAC)
  :commands (zeal-at-point zeal-at-point-set-docset)
  :init
  (defmacro def-docset! (mode docset)
    `(add-hook! ,mode (setq-local zeal-at-point-docset ,docset)))
  (defun doom-docs-lookup (&optional search docset)
    (let ((zeal-at-point-docset (or docset zeal-at-point-docset)))
      (zeal-at-point search))))

(provide 'core-docs)
;;; core-docs.el ends here
