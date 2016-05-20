;;; defuns-evil.el
;; for ../core-evil.el

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil" nil t)
(evil-define-command narf/evil-open-folds (count)
  "Instead of `evil-open-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-open-folds)))

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil" nil t)
(evil-define-command narf/evil-close-folds (count)
  "Instead of `evil-close-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-close-folds)))

;;;###autoload (autoload 'narf/multi-next-line "defuns-evil" nil t)
(evil-define-motion narf/multi-next-line (count)
  "Move down 6 lines"
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload 'narf/multi-previous-line "defuns-evil" nil t)
(evil-define-motion narf/multi-previous-line (count)
  "Move up 6 lines"
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload
(defun narf/evil-visual-line-state-p ()
  "Returns non-nil if in visual-line mode, nil otherwise."
  (and (evil-visual-state-p)
       (eq (evil-visual-type) 'line)))

;;;###autoload
(defun narf*evil-exchange-off ()
  (when evil-exchange--overlays
    (evil-exchange-cancel)))

;;;###autoload (autoload 'narf/evil-macro-on-all-lines "defuns-evil" nil t)
(evil-define-operator narf/evil-macro-on-all-lines (beg end &optional macro)
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
(defvar narf-buffer-match-global evil-ex-substitute-global "")
(defun narf--evil-ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-substitute-matches)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq flag 'update))

     ((eq flag 'stop)
      (evil-ex-delete-hl name)))))

(defun narf--evil-ex-buffer-match (arg &optional hl-name flags beg end)
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
(defun narf/evil-ex-buffer-match (flag &optional arg)
  (let ((hl-name 'evil-ex-buffer-match))
    (with-selected-window (minibuffer-selected-window)
      (narf--evil-ex-match-init hl-name)
      (narf--evil-ex-buffer-match arg hl-name (list (if narf-buffer-match-global ?g))))))

;;;###autoload
(defun narf/evil-ex-global-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-match))
    (with-selected-window (minibuffer-selected-window)
      (narf--evil-ex-match-init hl-name)
      (let ((result (car-safe (evil-ex-parse-global arg))))
        (narf--evil-ex-buffer-match result hl-name nil (point-min) (point-max))))))

;;;###autoload
(defun narf/evil-ex-undefine-cmd (cmd)
  (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
      (let ((abbrev (replace-match "" nil t cmd 1))
            (full (replace-match "\\2" nil nil cmd 1)))
        (setq evil-ex-commands (delq (assoc full evil-ex-commands) evil-ex-commands))
        (setq evil-ex-commands (delq (assoc abbrev evil-ex-commands) evil-ex-commands)))
    (setq evil-ex-commands (delq (assoc cmd evil-ex-commands) evil-ex-commands))))

(defvar narf:map-maps '())

;;;###autoload (autoload 'narf:map "defuns-evil" nil t)
(evil-define-command narf:map (bang input &optional mode)
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

;;;###autoload (autoload 'narf:nmap "defuns-evil" nil t)
(evil-define-command narf:nmap (bang input &optional mode)
  (interactive "<!><a>") (narf:map bang input 'normal))

;;;###autoload (autoload 'narf:imap "defuns-evil" nil t)
(evil-define-command narf:imap (bang input &optional mode)
  (interactive "<!><a>") (narf:map bang input 'insert))

;;;###autoload (autoload 'narf:vmap "defuns-evil" nil t)
(evil-define-command narf:vmap (bang input &optional mode)
  (interactive "<!><a>") (narf:map bang input 'visual))

;;;###autoload (autoload 'narf:mmap "defuns-evil" nil t)
(evil-define-command narf:mmap (bang input &optional mode)
  (interactive "<!><a>") (narf:map bang input 'motion))

;;;###autoload (autoload 'narf:omap "defuns-evil" nil t)
(evil-define-command narf:omap (bang input &optional mode)
  (interactive "<!><a>") (narf:map bang input 'operator))

;;;###autoload
(defun narf/evil-snipe-easymotion ()
  (interactive)
  (require 'evil-easymotion)
  (call-interactively narf--evil-snipe-repeat-fn))

(provide 'defuns-evil)
;;; defuns-evil.el ends here
