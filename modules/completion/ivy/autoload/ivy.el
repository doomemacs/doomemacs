;;; completion/ivy/autoload/ivy.el

;; Show more information in ivy-switch-buffer; and only display
;; workgroup-relevant buffers.
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
                   (list (concat
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
             (or buffer-list (doom-buffer-list))))))

(defun +ivy--select-buffer-action (buffer)
  (ivy--switch-buffer-action
   (s-chop-suffix
    "[+]"
    (substring buffer 0 (string-match-p (regexp-quote "   ") buffer)))))

(defun +ivy--select-buffer-other-window-action (buffer)
  (ivy--switch-buffer-other-window-action
   (s-chop-suffix
    "[+]"
    (substring buffer 0 (string-match-p (regexp-quote "   ") buffer)))))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional other-window-p)
  "Switch to an open buffer in the current workspace."
  (interactive "P")
  (+ivy/switch-buffer other-window-p t))

;;;###autoload
(defun +ivy/switch-buffer (&optional other-window-p workspace-only-p)
  "Switch to an open buffer in the global buffer list. If WORKSPACE-ONLY-P,
limit to buffers in the current workspace."
  (interactive "P")
  (ivy-read (format "%s buffers: " (if workspace-only-p "Workspace" "Global"))
            (+ivy--get-buffers (unless workspace-only-p (buffer-list)))
            :action (if other-window-p
                        '+ivy--select-buffer-other-window-action
                      '+ivy--select-buffer-action)
            :matcher 'ivy--switch-buffer-matcher
            :keymap ivy-switch-buffer-map
            :caller '+ivy/switch-workspace-buffer))

;;;###autoload
(defun +ivy/kill-ring ()
  "Search through the kill ring with `ivy'."
  (interactive)
  (ivy-read "Kill ring:"
            (cl-remove-if (lambda (it)
                            (or (< (length it) 3)
                                (string-match-p "\\`[\n[:blank:]]+\\'" it)))
                          (cl-remove-duplicates kill-ring :test 'equal))))

;;;###autoload
(defun +ivy/tasks ()
  "Search through all TODO/FIXME tags in the current project using
`counsel-rg'."
  (interactive)
  (counsel-rg "\\(TODO|FIXME\\)\\s" (doom-project-root) "--case-sensitive -w"))

;;;###autoload
(defun +ivy*counsel-ag-function (string base-cmd extra-ag-args)
  "Advice to 1) get rid of the character limit from `counsel-ag-function' and 2)
disable ivy's over-zealous parentheses quoting behavior, both of which
interferes with my custom :[ar]g ex command `+ivy:file-search'."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 1)  ;; #1
      (counsel-more-chars 1)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex
                         (counsel-unquote-regex-parens string)))))) ;; #2
      (let* ((args-end (string-match " -- " extra-ag-args))
             (file (if args-end
                       (substring-no-properties extra-ag-args (+ args-end 3))
                     ""))
             (extra-ag-args (if args-end
                                (substring-no-properties extra-ag-args 0 args-end)
                              extra-ag-args))
             (ag-cmd (format base-cmd
                             (concat extra-ag-args
                                     " -- "
                                     (shell-quote-argument regex)
                                     file))))
        (if (file-remote-p default-directory)
            (split-string (shell-command-to-string ag-cmd) "\n" t)
          (counsel--async-command ag-cmd)
          nil)))))

;;;###autoload
(defun +ivy/wgrep-occur ()
  "Invoke the search+replace wgrep buffer on the current ag/rg search results."
  (interactive)
  (if (not (window-minibuffer-p))
      (user-error "No completion session is active")
    (require 'wgrep)
    (let* ((caller (ivy-state-caller ivy-last))
           (occur-fn (plist-get ivy--occurs-list caller))
           (buffer
            (generate-new-buffer
             (format "*ivy-occur%s \"%s\"*"
                     (if caller (concat " " (prin1-to-string caller)) "")
                     ivy-text))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (funcall occur-fn))
        (setf (ivy-state-text ivy-last) ivy-text)
        (setq ivy-occur-last ivy-last)
        (setq-local ivy--directory ivy--directory))
      (ivy-exit-with-action
       `(lambda (_)
          (pop-to-buffer ,buffer)
          (ivy-wgrep-change-to-wgrep-mode))))))

;;;###autoload
(defun +ivy-yas-prompt (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))

;;;###autoload
(defun +ivy-git-grep-other-window-action (x)
  "Opens the current candidate in another window."
  (let (dest-window)
    (cl-letf (((symbol-function 'find-file)
               (lambda (filename)
                 (find-file-other-window filename)
                 (setq dest-window (selected-window)))))
      (counsel-git-grep-action x)
      (select-window dest-window))))
