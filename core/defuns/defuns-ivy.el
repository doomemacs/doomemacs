;;; defuns-ivy.el

;;;###autoload
(defun doom/ivy-switch-buffer (&optional all-p)
  "Displays open buffers in current project. If ALL-P, then show all open
buffers."
  (interactive)
  (let ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: " (doom/get-buffer-names (not all-p))
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'doom/ivy-switch-buffer)))

;;;###autoload
(defun doom/ivy-kill-ring ()
  (interactive)
  (ivy-read "Kill ring:" (--filter (not (or (< (length it) 3)
                                            (string-match-p "\\`[\n[:blank:]]+\\'" it)))
                                   (remove-duplicates kill-ring :test 'equal))))

;;;###autoload (autoload 'doom:ivy-recentf "defuns-ivy" nil t)
(evil-define-command doom:ivy-recentf (&optional bang)
  "Ex-mode interface for `ivy-recentf' and `projectile-recentf'."
  :repeat nil
  (interactive "<!>")
  (if bang (ivy-recentf) (projectile-recentf)))

;;;###autoload (autoload 'doom:ivy-swipe "defuns-ivy" nil t)
(evil-define-command doom:ivy-swiper (&optional search)
  (interactive "<a>")
  (swiper (or search (thing-at-point 'symbol))))

(defvar doom-ivy-ag-last-search nil)

;;;###autoload (autoload 'doom:ivy-ag-search "defuns-ivy" nil t)
(evil-define-operator doom:ivy-ag-search (beg end search regex-p &optional dir)
  "Preform a counsel search with SEARCH. If SEARCH is nil and in visual mode,
use the selection, otherwise activate live ag searching in helm.

If REGEX-P is non-nil, SEARCH will be treated as a regular expression.
DIR specifies the default-directory from which ag is run."
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (let ((counsel-ag-base-command
         (format "ag --nocolor --nogroup %s %%s -- ."
                 (if regex-p "-Q" "")))
        (search (or search
                    (and (evil-visual-state-p)
                         (and beg end (rxt-quote-pcre (buffer-substring-no-properties beg end))))
                    doom-ivy-ag-last-search)))
    (setq doom-ivy-ag-last-search search)
    (counsel-ag search (or dir (f-slash (doom/project-root))))))

;;;###autoload (autoload 'doom:ivy-ag-search-cwd "defuns-ivy" nil t)
(evil-define-operator doom:ivy-ag-search-cwd (beg end search regex-p)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (doom:ivy-ag-search beg end search regex-p default-directory))

;;;###autoload
(defun doom*counsel-ag-function (string)
  "Advice to get rid of the character limit from `counsel-ag-function', which
interferes with my custom :ag ex command `doom:ivy-ag-search'."
  (if (< (length string) 1)
      (counsel-more-chars 1)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re (ivy--regex string)))))
      (counsel--async-command
       (format counsel-ag-base-command (shell-quote-argument regex)))
      nil)))

(provide 'defuns-ivy)
;;; defuns-ivy.el ends here
