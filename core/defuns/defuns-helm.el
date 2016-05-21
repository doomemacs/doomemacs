;;; defuns-helm.el

;;;###autoload (autoload 'doom:helm-recentf "defuns-helm" nil t)
(evil-define-command doom:helm-recentf (&optional bang)
  "Ex-mode interface for `helm-recentf' and `helm-projectile-recentf'. If
 `bang', then `search' is interpreted as regexp."
  :repeat nil
  (interactive "<!>")
  (if bang (helm-recentf) (helm-projectile-recentf)))

;; Ex-mode interface for `helm-ag'. If `bang', then `search' is interpreted as
;; regexp.
;;;###autoload (autoload 'doom:helm-ag-search "defuns-helm" nil t)
(evil-define-operator doom:helm-ag-search (beg end search regex-p &optional dir)
  "Preform an helm-ag search with SEARCH. If SEARCH is nil and in visual mode, use the
selection, otherwise activate live ag searching in helm.

If REGEX-P is non-nil, SEARCH will be treated as a regular expression.
DIR specifies the default-directory from which ag is run."
  :type inclusive
  :repeat nil
  (interactive "<r><a><!>")
  (require 'helm-ag)
  (let* ((helm-ag--default-directory (or dir (f-slash (doom/project-root))))
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
;;;###autoload (autoload 'doom:helm-ag-search-cwd "defuns-helm" nil t)
(evil-define-operator doom:helm-ag-search-cwd (beg end &optional search bang)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (doom:helm-ag-search beg end search bang default-directory))

;; Ex-mode interface for `helm-swoop', `helm-multi-swoop-all' (if `bang'), or
;; `helm-css-scss' and `helm-css-scss-multi' (if `bang') if major-mode is
;; `scss-mode'
;;;###autoload (autoload 'doom:helm-swoop "defuns-helm" nil t)
(evil-define-command doom:helm-swoop (&optional search bang)
  :repeat nil
  (interactive "<a><!>")
  (if bang (helm-multi-swoop-all search) (helm-swoop :$query search)))

;;;###autoload
(defun doom/helm-find-in-emacsd ()
  (interactive)
  (in! doom-emacs-dir (helm-projectile-find-file)))

;;;###autoload
(defun doom/helm-find-in-dotfiles ()
  (interactive)
  (in! (expand-file-name ".dotfiles" "~") (helm-projectile-find-file)))

;;;###autoload
(defun doom/helm-buffers-dwim (&optional all-p)
  "Displays open buffers in current project. If ALL-P, then show all open
buffers."
  (interactive)
  (let ((doom-helm-force-project-buffers (and (not all-p) (doom/project-p))))
    (helm-buffers-list)))

;;;###autoload
(defun doom*helm-replace-prompt (plist)
  (if (keywordp (car plist))
      (setq plist (plist-put plist :prompt helm-global-prompt))
    (setcar (nthcdr 2 plist) helm-global-prompt))
  plist)

;;;###autoload
(defun doom*helm-hide-header (source &optional force)
  (setq header-line-format nil)
  (doom|hide-mode-line))

;;;###autoload
(defun doom*helm-hide-source-header-maybe ()
  (if (<= (length helm-sources) 1)
      (set-face-attribute 'helm-source-header nil :height 0.1 :foreground "#111111")
    (set-face-attribute 'helm-source-header nil :height 1.0 :foreground doom-helm-header-fg)))

(provide 'defuns-helm)
;;; defuns-helm.el ends here
