;;; defuns-helm.el
;; see ../core-helm.el

;;;###autoload
(defun narf|projectile-invalidate-cache-maybe ()
  (when (narf/project-p)
    (projectile-invalidate-cache nil)))

;;;###autoload (autoload 'narf:helm-recentf "defuns-helm" nil t)
(evil-define-command narf:helm-recentf (&optional bang)
  "Ex-mode interface for `helm-recentf' and `helm-projectile-recentf'. If
 `bang', then `search' is interpreted as regexp."
  :repeat nil
  (interactive "<!>")
  (if bang (helm-recentf) (helm-projectile-recentf)))

;; Ex-mode interface for `helm-ag'. If `bang', then `search' is interpreted as
;; regexp.
;;;###autoload (autoload 'narf:helm-ag-search "defuns-helm" nil t)
(evil-define-operator narf:helm-ag-search (beg end &optional search hidden-files-p pwd-p regex-p)
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
          :input input)))

;;;###autoload (autoload 'narf:helm-ag-regex-search "defuns-helm" nil t)
(evil-define-operator narf:helm-ag-regex-search (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf:helm-ag-search beg end search bang nil t))

;;;###autoload (autoload 'narf:helm-ag-search-cwd "defuns-helm" nil t)
(evil-define-operator narf:helm-ag-search-cwd (beg end &optional search bang)
  ;; Ex-mode interface for `helm-do-ag'. If `bang', then `search' is interpreted
  ;; as regexp
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf:helm-ag-search beg end search bang t nil))

;;;###autoload (autoload 'narf:helm-ag-regex-search-cwd "defuns-helm" nil t)
(evil-define-operator narf:helm-ag-regex-search-cwd (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf:helm-ag-search beg end search bang t t))

;; Ex-mode interface for `helm-swoop', `helm-multi-swoop-all' (if `bang'), or
;; `helm-css-scss' and `helm-css-scss-multi' (if `bang') if major-mode is
;; `scss-mode'
;;;###autoload (autoload 'narf:helm-swoop "defuns-helm" nil t)
(evil-define-command narf:helm-swoop (&optional search bang)
  :repeat nil
  (interactive "<a><!>")
  (if bang (helm-multi-swoop-all search) (helm-swoop :$query search)))

;;;###autoload
(defun narf/helm-find-in-emacsd ()
  (interactive)
  (in! narf-emacs-dir (helm-projectile-find-file)))

;;;###autoload
(defun narf/helm-find-in-dotfiles ()
  (interactive)
  (in! (expand-file-name ".dotfiles" "~") (helm-projectile-find-file)))

;;;###autoload
(defun narf/helm-buffers-dwim (&optional all-p)
  "Displays open buffers in current project. If ALL-P, then show all open
buffers."
  (if (and (not all-p) (narf/project-p))
      (helm-projectile-switch-to-buffer)
    (helm-buffers-list)))

;;;###autoload
(defun narf/helm-org-find-files ()
  (interactive)
  (in! org-directory
    (let ((helm-ff-skip-boring-files t))
      (helm-find-files-1 org-directory))))


(provide 'defuns-helm)
;;; defuns-helm.el ends here
