;;; feature/evil/packages.el

;;;###autoload
(defun +evil/visual-indent ()
  "vnoremap < <gv"
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/visual-dedent ()
  "vnoremap > >gv"
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil*ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME. Modified to include other substitution
flags."
  (let ((regexp (concat "\\(?:^\\|[^\\\\]\\)"
                        "\\([#%@]\\)"
                        "\\(\\(?::\\(?:[phtreS~.]\\|g?s[^: $]+\\)\\)*\\)"))
        case-fold-search)
    ;; TODO Remove s.el dependency so I can offer it upstream
    (dolist (match (s-match-strings-all regexp file-name))
      (let ((flags (split-string (cl-caddr match) ":" t))
            (path (file-relative-name
                   (pcase (cadr match)
                     ("@" (doom-project-root))
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


;;
;; Custom argument handlers
;;

(defvar +evil--buffer-match-global evil-ex-substitute-global "")

(defun +evil--ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-substitute-matches)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq flag 'update))

     ((eq flag 'stop)
      (evil-ex-delete-hl name)))))

(defun +evil--ex-buffer-match (arg &optional hl-name flags beg end)
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
(defun +evil-ex-buffer-match (flag &optional arg)
  (let ((hl-name 'evil-ex-buffer-match))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (+evil--ex-buffer-match arg hl-name (list (if +evil--buffer-match-global ?g))))))

;;;###autoload
(defun +evil-ex-global-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-match))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (let ((result (car-safe (evil-ex-parse-global arg))))
        (+evil--ex-buffer-match result hl-name nil (point-min) (point-max))))))

;;;###autoload
(defun +evil-window-move (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
evil-window-move-* (e.g. `evil-window-move-far-left')"
  (let* ((this-window (get-buffer-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (doom-popup-p that-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (case direction
                   ('left 'evil-window-move-far-left)
                   ('right 'evil-window-move-far-right)
                   ('up 'evil-window-move-very-top)
                   ('down 'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil (cond ((eq direction 'up) 'above)
                                                  ((eq direction 'down) 'below)
                                                  (t direction))))
        (with-selected-window that-window
          (switch-to-buffer doom-buffer))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun +evil/window-move-left () "`+evil-window-move'"  (interactive) (+evil-window-move 'left))
;;;###autoload
(defun +evil/window-move-right () "`+evil-window-move'" (interactive) (+evil-window-move 'right))
;;;###autoload
(defun +evil/window-move-up () "`+evil-window-move'"    (interactive) (+evil-window-move 'up))
;;;###autoload
(defun +evil/window-move-down () "`+evil-window-move'"  (interactive) (+evil-window-move 'down))

;;;###autoload
(defun +evil/matchit-or-toggle-fold ()
  "If on a fold-able element, toggle the fold (`hs-toggle-hiding'). Otherwise,
if on a delimiter, jump to the matching one (`evilmi-jump-items')."
  (interactive)
  (if (ignore-errors (hs-already-hidden-p))
      (hs-toggle-hiding)
    (call-interactively 'evilmi-jump-items)))

;;;###autoload (autoload '+evil:macro-on-all-lines "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:macro-on-all-lines (beg end &optional macro)
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

;;;###autoload (autoload '+evil:open-folds-recursively "feature/evil/autoload/evil" nil t)
(evil-define-command +evil:open-folds-recursively (level)
  "Opens all folds recursively, up to LEVEL."
  (interactive "<c>")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if level (hs-hide-level level) (evil-open-folds)))

;;;###autoload (autoload '+evil:close-folds-recursively "feature/evil/autoload/evil" nil t)
(evil-define-command +evil:close-folds-recursively (level)
  "Closes all folds recursively, up to LEVEL."
  (interactive "<c>")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if level (hs-hide-level level) (evil-close-folds)))

;;;###autoload (autoload '+evil:retab "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:retab (&optional beg end)
  "Wrapper around `doom/retab'."
  :motion nil :move-point nil :type line
  (interactive "<r>")
  (doom/retab beg end))

;;;###autoload (autoload '+evil:narrow-buffer "feature/evil/autoload/evil" nil t)
(evil-define-command +evil:narrow-buffer (beg end &optional bang)
  "Wrapper around `doom-narrow-buffer'."
  :move-point nil
  (interactive "<r><!>")
  (doom-narrow-buffer beg end bang))
