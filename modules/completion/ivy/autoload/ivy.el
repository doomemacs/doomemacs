;;; completion/ivy/autoload/ivy.el

;; Show more information in ivy-switch-buffer; and only display
;; project/workgroup-relevant buffers.
(defun +ivy--get-buffers (&optional buffer-list)
  (let ((min-name 5)
        (min-mode 5)
        (proot (doom-project-root)))
    (mapcar
     (lambda (b) (format (format "%%-%ds %%-%ds %%s" min-name min-mode)
                    (nth 0 b)
                    (nth 1 b)
                    (or (nth 2 b) "")))
     (mapcar (lambda (b)
               (with-current-buffer b
                 (let ((buffer-name (buffer-name b))
                       (mode-name (symbol-name major-mode)))
                   (when (> (length buffer-name) min-name)
                     (setq min-name (+ (length buffer-name) 15)))
                   (when (> (length mode-name) min-mode)
                     (setq min-mode (+ (length mode-name) 3)))
                   (list
                    (concat
                     (propertize buffer-name
                                 'face (cond ((string-match-p "^ ?\\*" buffer-name)
                                              'font-lock-comment-face)
                                             ((not (string= proot (doom-project-root)))
                                              'font-lock-keyword-face)
                                             (buffer-read-only
                                              'error)))
                     (when (and buffer-file-name (buffer-modified-p))
                       (propertize "[+]" 'face 'doom-modeline-buffer-modified)))
                    (propertize mode-name 'face 'font-lock-constant-face)
                    (when buffer-file-name
                      (abbreviate-file-name (file-name-directory buffer-file-name)))))))
             (or buffer-list (doom-buffer-list t))))))

(defun +ivy--select-buffer-action (buffer)
  (ivy--switch-buffer-action
   (s-chop-suffix
    "[+]"
    (substring buffer 0 (s-index-of "   " buffer)))))

;;;###autoload
(defun +ivy/switch-project-buffer (&optional all-p)
  "Displays open buffers in current project and workspace. If ALL-P, then show
all open buffers."
  (interactive)
  (ivy-read (format "%s buffers: " (if all-p "All" "Project"))
            (+ivy--get-buffers (if all-p (buffer-list)))
            :matcher #'ivy--switch-buffer-matcher
            :action #'+ivy--select-buffer-action
            :keymap ivy-switch-buffer-map
            :caller '+ivy/switch-project-buffer))

;;;###autoload
(defun +ivy/switch-buffer ()
  "Displays all open buffers, across projects and workspaces."
  (interactive)
  (+ivy/switch-project-buffer t))

;;;###autoload
(defun +ivy/kill-ring ()
  (interactive)
  (ivy-read "Kill ring:"
            (cl-remove-if (lambda (it)
                            (or (< (length it) 3)
                                (string-match-p "\\`[\n[:blank:]]+\\'" it)))
                          (cl-remove-duplicates kill-ring :test 'equal))))

;;;###autoload
(defun +ivy/tasks ()
  (interactive)
  ;; TODO Make nicer
  (counsel-ag " (TODO|FIXME|NOTE) " (doom-project-root)))

;;;###autoload
(defun +ivy*counsel-ag-function (string base-cmd extra-ag-args)
  "Advice to get rid of the character limit from `counsel-ag-function', which
interferes with my custom :ag ex command `+ivy:ag-search'."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 1)
      (counsel-more-chars 1)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let ((ag-cmd (format base-cmd
                            (concat extra-ag-args
                                    " -- "
                                    (shell-quote-argument regex)))))
        (if (file-remote-p default-directory)
            (split-string (shell-command-to-string ag-cmd) "\n" t)
          (counsel--async-command ag-cmd)
          nil)))))

;;;###autoload
(defun +ivy/counsel-ag-occur ()
  "Invoke the search+replace wgrep buffer on the current ag search results."
  (interactive)
  (require 'wgrep)
  (call-interactively 'ivy-occur))

;;;###autoload
(defun +ivy-yas-prompt (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))
