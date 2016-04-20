;;; defuns-helm.el
;; see ../core-helm.el

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
(evil-define-operator narf:helm-ag-search (beg end search regex-p &optional dir)
  "Preform an helm-ag search with SEARCH. If SEARCH is nil and in visual mode, use the
selection, otherwise activate live ag searching in helm.

If REGEX-P is non-nil, SEARCH will be treated as a regular expression.
DIR specifies the default-directory from which ag is run."
  :type inclusive
  :repeat nil
  (interactive "<r><a><!>")
  (require 'helm-ag)
  (let* ((helm-ag--default-directory (or dir (f-slash (narf/project-root))))
         (helm-ag-command-option (unless regex-p "-Q "))
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

;; Ex-mode interface for `helm-do-ag'. If `bang', then `search' is interpreted
;; as regexp
;;;###autoload (autoload 'narf:helm-ag-search-cwd "defuns-helm" nil t)
(evil-define-operator narf:helm-ag-search-cwd (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (narf:helm-ag-search beg end search bang default-directory))

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
  (interactive)
  (let ((narf-helm-force-project-buffers (and (not all-p) (narf/project-p))))
    (helm-buffers-list)))

(provide 'defuns-helm)
;;; defuns-helm.el ends here
