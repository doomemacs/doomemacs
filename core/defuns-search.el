;;;###autoload
(defun narf:ido-find-file (&optional dir)
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (ido-find-file)))

;;;###autoload
(defun narf:ido-find-file-other-window (&optional dir)
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (ido-find-file-other-window)))

;;;###autoload
(defun narf:ido-find-project-file ()
  (interactive)
  (let ((default-directory (narf/project-root)))
    (ido-find-file)))

;;;###autoload (autoload 'narf::initfiles "defuns-search")
(evil-define-command narf::initfiles (&optional bang) :repeat nil
  (interactive "<!>")
  (if bang
      (ido-find-file-in-dir MODULES-DIR)
    (ido-find-file-in-dir BASE-DIR)))

;;;###autoload (autoload 'narf::notes "defuns-search")
(evil-define-command narf::notes () :repeat nil
  (interactive)
  (require 'org)
  (ido-find-file-in-dir org-directory))

;; Ex-mode interface for `helm-recentf' and `helm-projectile-recentf'. If
;; `bang', then `search' is interpreted as regexp
;;;###autoload (autoload 'narf::recentf "defuns-search")
(evil-define-command narf::recentf (&optional bang)
  :repeat nil
  (interactive "<!>")
  (if bang (helm-recentf) (helm-projectile-recentf)))

;; Ex-mode interface for `helm-ag'. If `bang', then `search' is interpreted as
;; regexp.
;;;###autoload (autoload 'narf::ag-search "defuns-search")
(evil-define-operator narf::ag-search (beg end &optional search hidden-files-p pwd-p regex-p)
  :type inclusive
  :repeat nil
  (interactive "<r><a><!>")
  (helm-alive-p)
  (let* ((helm-ag-default-directory (if pwd-p default-directory (narf/project-root)))
         (helm-ag-command-option (concat (unless regex-p "-Q ")
                                         (if hidden-files-p "--hidden ")))
         (input "")
         (header-name (format "Search in %s" helm-ag-default-directory)))
    (if search
        (progn
          (helm-attrset 'search-this-file nil helm-ag-source)
          (setq helm-ag--last-query search))
      (if (and beg end (/= beg (1- end)))
          (setq input (buffer-substring-no-properties beg end))))
    (helm-attrset 'name header-name helm-ag-source)
    (helm :sources (if search (helm-ag--select-source) '(helm-source-do-ag))
          :buffer "*helm-ag*"
          :input input
          :prompt helm-global-prompt)))

;;;###autoload (autoload 'narf::ag-regex-search "defuns-search")
(evil-define-operator narf::ag-regex-search (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf::ag-search beg end search bang nil t))

;;;###autoload (autoload 'narf::ag-regex-cwd "defuns-search")
(evil-define-operator narf::ag-search-cwd (beg end &optional search bang)
  ;; Ex-mode interface for `helm-do-ag'. If `bang', then `search' is interpreted
  ;; as regexp
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf::ag-search beg end search bang t nil))

;;;###autoload (autoload 'narf::ag-regex-search-cwd "defuns-search")
(evil-define-operator narf::ag-regex-search-cwd (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf::ag-search beg end search bang t t))

;; Ex-mode interface for `helm-swoop', `helm-multi-swoop-all' (if `bang'), or
;; `helm-css-scss' and `helm-css-scss-multi' (if `bang') if major-mode is
;; `scss-mode'
;;;###autoload (autoload 'narf::swoop "defuns-search")
(evil-define-command narf::swoop (&optional search bang)
  :repeat nil
  (interactive "<a><!>")
  (if (eq major-mode 'scss-mode)
      (if bang (helm-css-scss-multi search) (helm-css-scss search))
    (if bang (helm-multi-swoop-all search) (helm-swoop :$query search))))

;; TODO: Implement helm-find-file
;;;###autoload (autoload 'narf::snippets "defuns-search")
(evil-define-command narf::snippets (&optional bang)
  (interactive "<!><a>")
  (if bang
      (narf:ido-find-file SNIPPETS-DIR)
    (helm-yas-visit-snippet-file)))


(provide 'defuns-search)
;;; defuns-search.el ends here
