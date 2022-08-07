;; editor/evil/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-evil-initial-state! (modes state)
  "Set the initialize STATE of MODES using `evil-set-initial-state'."
  (declare (indent defun))
  (after! evil
    (if (listp modes)
        (dolist (mode (ensure-list modes))
          (evil-set-initial-state mode state))
      (evil-set-initial-state modes state))))


;;
;;; Interactive commands

;;;###autoload
(defun +evil/shift-right ()
  "vnoremap < <gv"
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/shift-left ()
  "vnoremap > >gv"
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/alt-paste ()
  "Call `evil-paste-after' but invert `evil-kill-on-visual-paste'.
By default, this replaces the selection with what's in the clipboard without
replacing its contents."
  (interactive)
  (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
    (call-interactively #'evil-paste-after)))

(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
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
      (window-swap-states this-window that-window)
      (select-window that-window))))

;;;###autoload
(defun +evil/window-move-left ()
  "Swap windows to the left."
  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun +evil/window-move-right ()
  "Swap windows to the right"
  (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun +evil/window-move-up ()
  "Swap windows upward."
  (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun +evil/window-move-down ()
  "Swap windows downward."
  (interactive) (+evil--window-swap 'down))

;;;###autoload
(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;;;###autoload
(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

;;;###autoload (autoload '+evil:apply-macro "editor/evil/autoload/evil" nil t)
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

;;;###autoload (autoload '+evil:retab "editor/evil/autoload/evil" nil t)
(evil-define-operator +evil:retab (&optional beg end)
  "Wrapper around `doom/retab'."
  :motion nil :move-point nil :type line
  (interactive "<r>")
  (doom/retab nil beg end))

;;;###autoload (autoload '+evil:narrow-buffer "editor/evil/autoload/evil" nil t)
(evil-define-operator +evil:narrow-buffer (beg end &optional bang)
  "Narrow the buffer to region between BEG and END.

Widens narrowed buffers first. If BANG, use indirect buffer clones instead."
  :move-point nil
  (interactive "<r><!>")
  (if (not bang)
      (if (buffer-narrowed-p)
          (widen)
        (narrow-to-region beg end))
    (when (buffer-narrowed-p)
      (doom/widen-indirectly-narrowed-buffer t))
    (doom/narrow-buffer-indirectly beg end)))

;;;###autoload (autoload '+evil:yank-unindented "editor/evil/autoload/evil" nil t)
(evil-define-operator +evil:yank-unindented (beg end _type _register _yank-handler)
  "Saves the (reindented) characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((indent (save-excursion (goto-char beg) (current-indentation)))
        (text (buffer-substring beg end)))
    (with-temp-buffer
      (insert text)
      (indent-rigidly (point-min) (point-max) (- indent))
      (evil-yank (point-min) (point-max)))))


;;
;;; wgrep

;;;###autoload (autoload '+evil-delete "editor/evil/autoload/evil" nil t)
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
