;; feature/evil/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autodef
(defun set-evil-initial-state! (modes state)
  "Set the initialize STATE of MODES using `evil-set-initial-state'."
  (declare (indent defun))
  (after! evil
    (if (listp modes)
        (dolist (mode (doom-enlist modes))
          (evil-set-initial-state mode state))
      (evil-set-initial-state modes state))))

;; FIXME obsolete :evil-state
;;;###autoload
(def-setting! :evil-state (modes state)
  :obsolete set-evil-initial-state!
  `(set-evil-initial-state! ,modes ,state))


;;
;; Commands

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
(defun +evil/reselect-paste ()
  "Return to visual mode and reselect the last pasted region."
  (interactive)
  (cl-destructuring-bind (_ _ _ beg end &optional _)
      evil-last-paste
    (evil-visual-make-selection
     (save-excursion (goto-char beg) (point-marker))
     end)))

;;;###autoload
(defun +evil/paste-preserve-register ()
  "Call `evil-paste-after' without overwriting the clipboard (by writing to the
0 register instead). This allows you to paste the same text again afterwards."
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively #'evil-paste-after)))

(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
evil-window-move-* (e.g. `evil-window-move-far-left')"
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (pcase direction
                   ('left  #'evil-window-move-far-left)
                   ('right #'evil-window-move-far-right)
                   ('up    #'evil-window-move-very-top)
                   ('down  #'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil
                            (pcase direction
                              ('up 'above)
                              ('down 'below)
                              (_ direction))))
        (with-selected-window that-window
          (switch-to-buffer (doom-fallback-buffer)))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun +evil/window-move-left () "See `+evil--window-swap'"  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun +evil/window-move-right () "See `+evil--window-swap'" (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun +evil/window-move-up () "See `+evil--window-swap'"    (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun +evil/window-move-down () "See `+evil--window-swap'"  (interactive) (+evil--window-swap 'down))

;;;###autoload
(defun +evil/easymotion ()
  "Invoke and lazy-load `evil-easymotion' without compromising which-key
integration."
  (interactive)
  (let ((prefix (this-command-keys)))
    (evil-define-key* 'motion 'global prefix nil)
    (evilem-default-keybindings prefix)
    (which-key-reload-key-sequence
     (vconcat (when evil-this-operator
                (where-is-internal evil-this-operator
                                   evil-normal-state-map
                                   t))
              prefix))))


;;
;; Evil commands/operators

;;;###autoload (autoload '+evil:apply-macro "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:apply-macro (beg end)
  "Apply macro to each line."
  :move-point nil
  (interactive "<r>")
  (let ((register (or evil-this-register (read-char)))
        macro)
    (cond ((or (and (eq register ?@) (eq evil-last-register ?:))
               (eq register ?:))
           (setq macro (lambda () (evil-ex-repeat nil))
                 evil-last-register ?:))
          ((eq register ?@)
           (unless evil-last-register
             (user-error "No previously executed keyboard macro."))
           (setq macro (evil-get-register evil-last-register t)))
          ((setq macro (evil-get-register register t)
                 evil-last-register register)))
    (unless macro
      (user-error "No macro recorded in %c register" register))
    (evil-change-state 'normal)
    (evil-with-single-undo
      (let ((lines (count-lines beg end)))
        (message "Applied macro in %c register %d times" register lines)
        (apply-macro-to-region-lines beg end macro)
        (message "Applied macro in %c register %d times...DONE" register lines)))))

;;;###autoload (autoload '+evil:retab "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:retab (&optional beg end)
  "Wrapper around `doom/retab'."
  :motion nil :move-point nil :type line
  (interactive "<r>")
  (doom/retab beg end))

;;;###autoload (autoload '+evil:narrow-buffer "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:narrow-buffer (beg end &optional bang)
  "Wrapper around `doom/clone-and-narrow-buffer'."
  :move-point nil
  (interactive "<r><!>")
  (doom/clone-and-narrow-buffer beg end bang))


;; --- custom arg handlers ----------------

(defvar +evil--flag nil)

(defun +evil--ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq +evil--flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-substitute-matches)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq +evil--flag 'update))

     ((eq +evil--flag 'stop)
      (evil-ex-delete-hl name)))))

(defun +evil--ex-buffer-match (arg &optional hl-name flags beg end)
  (when (and (eq +evil--flag 'update)
             evil-ex-substitute-highlight-all
             (not (zerop (length arg))))
    (condition-case lossage
        (let ((pattern (evil-ex-make-substitute-pattern
                        arg
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
  (let ((hl-name 'evil-ex-buffer-match)
        (+evil--flag flag))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (+evil--ex-buffer-match arg hl-name (list (if evil-ex-substitute-global ?g))))))

;;;###autoload
(defun +evil-ex-global-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-match)
        (+evil--flag flag))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (+evil--ex-buffer-match arg hl-name nil (point-min) (point-max)))))

;;;###autoload
(defun +evil-ex-global-delim-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-delim-match)
        (+evil--flag flag))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (let ((result (car-safe (evil-delimited-arguments arg 2))))
        (+evil--ex-buffer-match result hl-name nil (point-min) (point-max))))))

;;;###autoload (autoload '+evil:align "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:align (beg end pattern &optional bang)
  "Ex interface to `align-regexp'. PATTERN is a vim-style regexp. If BANG,
repeat the alignment for all matches (otherwise just the first match on each
line)."
  (interactive "<r><//g><!>")
  (align-regexp
   beg end
   (concat "\\(\\s-*\\)" (evil-transform-vim-style-regexp pattern))
   1 1 bang))

;;;###autoload (autoload '+evil:align-right "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:align-right (beg end pattern &optional bang)
  "Like `+evil:align', except alignments are right-justified. PATTERN is a
vim-style regexp. If BANG, repeat the alignment for all matches (otherwise just
the first match on each line)."
  (interactive "<r><//g><!>")
  (align-regexp
   beg end
   (concat "\\(" (evil-transform-vim-style-regexp pattern) "\\)")
   -1 1 bang))


;; --- wgrep ------------------------------

;;;###autoload (autoload '+evil-delete "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil-delete (beg end type register yank-handler)
  "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
  (interactive "<R><x><y>")
  (condition-case _ex
      (evil-delete beg end type register yank-handler)
    ('text-read-only
     (evil-apply-on-block
      (lambda (beg _)
        (goto-char beg)
        (call-interactively #'wgrep-mark-deletion))
      beg (1- end) nil))))
