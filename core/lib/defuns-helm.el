;;; defuns-helm.el
;; see ../core-helm.el

;;;###autoload
(defun narf|projectile-invalidate-cache-maybe ()
  (when (narf/project-p)
    (projectile-invalidate-cache nil)))

;;;###autoload
(defun narf*projectile-replace-prompt (&optional string)
  "Don't show the project name in the prompts; I already know."
  helm-global-prompt)

;;;###autoload
(defun narf*helm-hide-modeline (source &optional force)
  "No persistent header."
  (setq mode-line-format nil)
  (setq header-line-format nil))

;;;###autoload
(defun narf/helm-get-org-candidates-in-file (filename min-depth max-depth &optional fontify nofname)
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename)))
    (and fontify (jit-lock-fontify-now))
    (let ((match-fn (if fontify 'match-string 'match-string-no-properties)))
      (save-excursion
        (goto-char (point-min))
        (cl-loop with width = (window-width)
                 while (re-search-forward org-complex-heading-regexp nil t)
                 if (let ((num-stars (length (match-string-no-properties 1))))
                      (and (>= num-stars min-depth) (<= num-stars max-depth)))
                 collect `(,(let ((heading (funcall match-fn 4))
                                  (file (unless nofname
                                          (concat (f-no-ext (f-relative filename org-directory)) ":")))
                                  (level (length (match-string-no-properties 1))))
                              (org-format-outline-path
                               (append (org-get-outline-path t level heading)
                                       (list heading)) width file))
                           . ,(point-marker)))))))

;;;###autoload (autoload 'narf:helm-recentf "defuns-helm" nil t)
(evil-define-command narf:helm-recentf (&optional bang)
  "Ex-mode interface for `helm-recentf' and `helm-projectile-recentf'. If
 `bang', then `search' is interpreted as regexp."
  :repeat nil
  (interactive "<!>")
  (if bang (helm-recentf) (helm-projectile-recentf)))

;; Ex-mode interface for `helm-ag'. If `bang', then `search' is interpreted as
;; regexp.
;;;###autoload (autoload 'narf:helm-search "defuns-helm" nil t)
(evil-define-operator narf:helm-search (beg end &optional search hidden-files-p pwd-p regex-p)
  :type inclusive
  :repeat nil
  (interactive "<r><a><!>")
  (require 'helm-ag)
  (let* ((helm-ag--default-directory (if pwd-p default-directory (concat (narf/project-root) "/")))
         (helm-ag-command-option (concat (unless regex-p "-Q ")
                                         (if hidden-files-p "--hidden ")))
         (input "")
         (header-name (format "Search in %s" helm-ag--default-directory)))
    (if search
        (progn
          (helm-attrset 'search-this-file nil helm-ag-source)
          (setq helm-ag--last-query search))
      (if (and beg end (/= beg (1- end)))
          (setq input (buffer-substring-no-properties beg end))))
    (helm-attrset 'name header-name helm-ag-source)
    (helm :sources (if search helm-ag-source '(helm-source-do-ag))
          :buffer "*helm-ag*"
          :keymap helm-ag-map
          :input input
          :prompt helm-global-prompt)))

;;;###autoload (autoload 'narf:helm-regex-search "defuns-helm" nil t)
(evil-define-operator narf:helm-regex-search (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf:helm-search beg end search bang nil t))

;;;###autoload (autoload 'narf:helm-regex-cwd "defuns-helm" nil t)
(evil-define-operator narf:helm-search-cwd (beg end &optional search bang)
  ;; Ex-mode interface for `helm-do-ag'. If `bang', then `search' is interpreted
  ;; as regexp
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf:helm-search beg end search bang t nil))

;;;###autoload (autoload 'narf:helm-regex-search-cwd "defuns-helm" nil t)
(evil-define-operator narf:helm-regex-search-cwd (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf:helm-search beg end search bang t t))

;; Ex-mode interface for `helm-swoop', `helm-multi-swoop-all' (if `bang'), or
;; `helm-css-scss' and `helm-css-scss-multi' (if `bang') if major-mode is
;; `scss-mode'
;;;###autoload (autoload 'narf:helm-swoop "defuns-helm" nil t)
(evil-define-command narf:helm-swoop (&optional search bang)
  :repeat nil
  (interactive "<a><!>")
  (if bang (helm-multi-swoop-all search) (helm-swoop :$query search)))

(provide 'defuns-helm)
;;; defuns-helm.el ends here
