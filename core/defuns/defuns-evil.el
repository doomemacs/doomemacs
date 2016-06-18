;;; defuns-evil.el

;;;###autoload (autoload 'doom:evil-open-folds "defuns-evil" nil t)
(evil-define-command doom/evil-open-folds (count)
  "Instead of `evil-open-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-open-folds)))

;;;###autoload (autoload 'doom:evil-open-folds "defuns-evil" nil t)
(evil-define-command doom/evil-close-folds (count)
  "Instead of `evil-close-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-close-folds)))

;;;###autoload (autoload 'doom/multi-next-line "defuns-evil" nil t)
(evil-define-motion doom/multi-next-line (count)
  "Move down 6 lines"
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload 'doom/multi-previous-line "defuns-evil" nil t)
(evil-define-motion doom/multi-previous-line (count)
  "Move up 6 lines"
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload
(defun doom/evil-visual-line-state-p ()
  "Returns non-nil if in visual-line mode, nil otherwise."
  (and (evil-visual-state-p)
       (eq (evil-visual-type) 'line)))

;;;###autoload
(defun doom*evil-exchange-off ()
  (when evil-exchange--overlays
    (evil-exchange-cancel)))

;;;###autoload (autoload 'doom/evil-macro-on-all-lines "defuns-evil" nil t)
(evil-define-operator doom/evil-macro-on-all-lines (beg end &optional macro)
  "Apply macro to each line."
  :motion nil
  :move-point nil
  (interactive "<r><a>")
  (unless (and beg end)
    (setq beg (region-beginning)
          end (region-end)))
  (evil-ex-normal beg end
                  (concat "@"
                          (single-key-description
                           (or macro (read-char "@-"))))))

;;; Custom argument handlers
(defvar doom-buffer-match-global evil-ex-substitute-global "")
(defun doom--evil-ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-substitute-matches)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq flag 'update))

     ((eq flag 'stop)
      (evil-ex-delete-hl name)))))

(defun doom--evil-ex-buffer-match (arg &optional hl-name flags beg end)
  (when (and (eq flag 'update)
             evil-ex-substitute-highlight-all
             (not (zerop (length arg))))
    (condition-case lossage
        (let ((pattern (evil-ex-make-substitute-pattern
                        (if evil-ex-bang (regexp-quote arg) arg)
                        (or flags (list))))
              (range (or (evil-copy-range evil-ex-range)
                         (evil-range (or beg (line-beginning-position))
                                     (or end (line-end-position))
                                     'line
                                     :expanded t))))
          (evil-expand-range range)
          (evil-ex-hl-set-region hl-name
                                 (max (evil-range-beginning range) (window-start))
                                 (min (evil-range-end range) (window-end)))
          (evil-ex-hl-change hl-name pattern))
      (end-of-file
       (evil-ex-pattern-update-ex-info nil "incomplete replacement"))
      (user-error
       (evil-ex-pattern-update-ex-info nil (format "?%s" lossage))))))

;;;###autoload
(defun doom/evil-ex-buffer-match (flag &optional arg)
  (let ((hl-name 'evil-ex-buffer-match))
    (with-selected-window (minibuffer-selected-window)
      (doom--evil-ex-match-init hl-name)
      (doom--evil-ex-buffer-match arg hl-name (list (if doom-buffer-match-global ?g))))))

;;;###autoload
(defun doom/evil-ex-global-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-match))
    (with-selected-window (minibuffer-selected-window)
      (doom--evil-ex-match-init hl-name)
      (let ((result (car-safe (evil-ex-parse-global arg))))
        (doom--evil-ex-buffer-match result hl-name nil (point-min) (point-max))))))

;;;###autoload
(defun doom/evil-ex-undefine-cmd (cmd)
  (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
      (let ((abbrev (replace-match "" nil t cmd 1))
            (full (replace-match "\\2" nil nil cmd 1)))
        (setq evil-ex-commands (delq (assoc full evil-ex-commands) evil-ex-commands))
        (setq evil-ex-commands (delq (assoc abbrev evil-ex-commands) evil-ex-commands)))
    (setq evil-ex-commands (delq (assoc cmd evil-ex-commands) evil-ex-commands))))

(defvar doom:map-maps '())

;;;###autoload (autoload 'doom:map "defuns-evil" nil t)
(evil-define-command doom:map (bang input &optional mode)
  "Map ex commands to keybindings. INPUT should be in the format [KEY] [EX COMMAND]."
  (interactive "<!><a>")
  (let* ((parts (s-split-up-to " " input 2 t))
         (mode (or mode 'normal))
         (key (kbd (car parts)))
         (command (s-join " " (cdr parts)))
         (map (cl-case mode
                ('normal evil-normal-state-local-map)
                ('insert evil-insert-state-local-map)
                ('visual evil-visual-state-local-map)
                ('motion evil-motion-state-local-map)
                ('operator evil-operator-state-local-map)))
         (fn `(lambda () (interactive) (evil-ex-eval ,command))))
    (if bang
        (evil-define-key mode nil key fn)
      (define-key map key fn))))

;;;###autoload (autoload 'doom:nmap "defuns-evil" nil t)
(evil-define-command doom:nmap (bang input &optional mode)
  (interactive "<!><a>") (doom:map bang input 'normal))

;;;###autoload (autoload 'doom:imap "defuns-evil" nil t)
(evil-define-command doom:imap (bang input &optional mode)
  (interactive "<!><a>") (doom:map bang input 'insert))

;;;###autoload (autoload 'doom:vmap "defuns-evil" nil t)
(evil-define-command doom:vmap (bang input &optional mode)
  (interactive "<!><a>") (doom:map bang input 'visual))

;;;###autoload (autoload 'doom:mmap "defuns-evil" nil t)
(evil-define-command doom:mmap (bang input &optional mode)
  (interactive "<!><a>") (doom:map bang input 'motion))

;;;###autoload (autoload 'doom:omap "defuns-evil" nil t)
(evil-define-command doom:omap (bang input &optional mode)
  (interactive "<!><a>") (doom:map bang input 'operator))

;;;###autoload
(defun doom/evil-snipe-easymotion ()
  (interactive)
  (require 'evil-easymotion)
  (call-interactively doom--evil-snipe-repeat-fn))

;;;###autoload
(defun doom/evil-matchit ()
  (interactive)
  (if (ignore-errors (hs-already-hidden-p))
      (hs-toggle-hiding)
    (call-interactively 'evilmi-jump-items)))

;;;###autoload
(defun doom*evil-command-window (hist cmd-key execute-fn)
  "The evil command window has a mind of its own (uses `switch-to-buffer'). We
monkey patch it to use pop-to-buffer."
  (when (eq major-mode 'evil-command-window-mode)
    (user-error "Cannot recursively open command line window"))
  (dolist (win (window-list))
    (when (equal (buffer-name (window-buffer win))
                 "*Command Line*")
      (kill-buffer (window-buffer win))
      (delete-window win)))
  (setq evil-command-window-current-buffer (current-buffer))
  (ignore-errors (kill-buffer "*Command Line*"))
  (with-current-buffer (pop-to-buffer "*Command Line*")
    (setq-local evil-command-window-execute-fn execute-fn)
    (setq-local evil-command-window-cmd-key cmd-key)
    (evil-command-window-mode)
    (evil-command-window-insert-commands hist)))

;;;###autoload
(defun doom*evil-esc-quit ()
  "Close popups, disable search highlights and quit the minibuffer if open."
  (let ((minib-p (minibuffer-window-active-p (minibuffer-window)))
        (evil-hl-p (evil-ex-hl-active-p 'evil-ex-search)))
    (when minib-p (abort-recursive-edit))
    (when evil-hl-p (evil-ex-nohighlight))
    ;; Close non-repl popups and clean up `doom-popup-windows'
    (unless (or minib-p evil-hl-p)
      (doom/popup-close-all))))

;;;###autoload
(defun doom*evil-ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME."
  (let ((regexp (concat "\\(?:^\\|[^\\\\]\\)"
                        "\\([#%@]\\)"
                        "\\(\\(?::\\(?:[phtreS~.]\\|g?s[^: $]+\\)\\)*\\)"))
        case-fold-search)
    (dolist (match (s-match-strings-all regexp file-name))
      (let ((flags (split-string (caddr match) ":" t))
            (path (file-relative-name
                   (pcase (cadr match)
                     ("@" (doom/project-root))
                     ("%" (buffer-file-name))
                     ("#" (and (other-buffer) (buffer-file-name (other-buffer)))))
                   default-directory))
            flag global)
        (when path
          (while flags
            (setq flag (pop flags))
            (when (string-suffix-p "\\" flag)
              (setq flag (concat flag (pop flags))))
            (when (string-prefix-p "gs" flag)
              (setq global t
                    flag (string-remove-prefix "g" flag)))
            (setq path
                  (or (pcase (substring flag 0 1)
                        ("p" (expand-file-name path))
                        ("~" (file-relative-name path "~"))
                        ("." (file-relative-name path default-directory))
                        ("h" (directory-file-name path))
                        ("t" (file-name-nondirectory (directory-file-name path)))
                        ("r" (file-name-sans-extension path))
                        ("e" (file-name-extension path))
                        ("s" (let* ((args (evil-delimited-arguments (substring flag 1) 2))
                                    (pattern (evil-transform-vim-style-regexp (car args)))
                                    (replace (cadr args)))
                               (replace-regexp-in-string
                                (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                (evil-transform-vim-style-regexp replace) path t t
                                (unless global 1))))
                        ("S" (shell-quote-argument path))
                        (_ path))
                      "")))
          (setq file-name
                (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                  (string-trim-left (car match)))
                                          path file-name t t 1)))))
    (setq file-name (replace-regexp-in-string regexp "\\1" file-name t))))

(provide 'defuns-evil)
;;; defuns-evil.el ends here
