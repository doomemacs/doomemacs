;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;;
;;; Public library

;;;###autoload
(defun doom-resize-window (window new-size &optional horizontal force-p)
  "Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise.
If FORCE-P is omitted when `window-size-fixed' is non-nil, resizing will fail."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
                      horizontal))))

;;;###autoload
(defun doom-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.

Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (or (not (ignore-errors (doom-real-buffer-list)))
      (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
      (ignore (message "Aborted"))))


;;
;;; Advice

;;;###autoload
(defun doom-recenter-a (&rest _)
  "Generic advisor for recentering window (typically :after other functions)."
  (recenter))

;;;###autoload
(defun doom-shut-up-a (orig-fn &rest args)
  "Generic advisor for silencing noisy functions.

In interactive Emacs, this just inhibits messages from appearing in the
minibuffer. They are still logged to *Messages*.

In tty Emacs, messages suppressed completely."
  (quiet! (apply orig-fn args)))


;;
;;; Hooks

;;;###autoload
(defun doom-apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))

;;;###autoload
(defun doom-disable-show-paren-mode-h ()
  "Turn off `show-paren-mode' buffer-locally."
  (setq-local show-paren-mode nil))

;;;###autoload
(defun doom-enable-line-numbers-h ()
  (display-line-numbers-mode +1))

;;;###autoload
(defun doom-disable-line-numbers-h ()
  (display-line-numbers-mode -1))


;;
;;; Commands

;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.

See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))

;;;###autoload
(defun doom/delete-frame ()
  "Delete the current frame, but ask for confirmation if it isn't empty."
  (interactive)
  (if (cdr (frame-list))
      (when (doom-quit-p "Close frame?")
        (delete-frame))
    (save-buffers-kill-emacs)))

(defvar doom--maximize-last-wconf nil)
;;;###autoload
(defun doom/window-maximize-buffer ()
  "Close other windows to focus on this one. Activate again to undo this. If the
window changes before then, the undo expires.

Alternatively, use `doom/window-enlargen'."
  (interactive)
  (setq doom--maximize-last-wconf
        (if (and (null (cdr (cl-remove-if #'window-dedicated-p (window-list))))
                 doom--maximize-last-wconf)
            (ignore (set-window-configuration doom--maximize-last-wconf))
          (when (and (bound-and-true-p +popup-mode)
                     (+popup-window-p))
            (user-error "Cannot maximize a popup, use `+popup/raise' first or use `doom/window-enlargen' instead"))
          (prog1 (current-window-configuration)
            (delete-other-windows)))))

(defvar doom--enlargen-last-wconf nil)
;;;###autoload
(defun doom/window-enlargen ()
  "Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-maximize-buffer'). Activate again to undo."
  (interactive)
  (setq doom--enlargen-last-wconf
        (if doom--enlargen-last-wconf
            (ignore (set-window-configuration doom--enlargen-last-wconf))
          (prog1 (current-window-configuration)
            (let* ((window (selected-window))
                   (dedicated-p (window-dedicated-p window))
                   (preserved-p (window-parameter window 'window-preserved-size))
                   (ignore-window-parameters t))
              (unwind-protect
                  (progn
                    (when dedicated-p
                      (set-window-dedicated-p window nil))
                    (when preserved-p
                      (set-window-parameter window 'window-preserved-size nil))
                    (maximize-window window))
                (set-window-dedicated-p window dedicated-p)
                (when preserved-p
                  (set-window-parameter window 'window-preserved-size preserved-p))))))))

;;;###autoload
(defun doom/window-maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-left)) (delete-window))
    (while (ignore-errors (windmove-right)) (delete-window))))

;;;###autoload
(defun doom/window-maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-up)) (delete-window))
    (while (ignore-errors (windmove-down)) (delete-window))))

;;;###autoload
(defun doom/set-frame-opacity (opacity)
  "Interactively change the current frame's opacity.

OPACITY is an integer between 0 to 100, inclusive."
  (interactive
   (list (read-number "Opacity (0-100): "
                      (or (frame-parameter nil 'alpha)
                          100))))
  (set-frame-parameter nil 'alpha opacity))

(defvar doom--narrowed-base-buffer nil)
;;;###autoload
(defun doom/narrow-buffer-indirectly (beg end)
  "Restrict editing in this buffer to the current region, indirectly.

This recursively creates indirect clones of the current buffer so that the
narrowing doesn't affect other windows displaying the same buffer. Call
`doom/widen-indirectly-narrowed-buffer' to undo it (incrementally).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive
   (list (or (bound-and-true-p evil-visual-beginning) (region-beginning))
         (or (bound-and-true-p evil-visual-end)       (region-end))))
  (unless (region-active-p)
    (setq beg (line-beginning-position)
          end (line-end-position)))
  (deactivate-mark)
  (let ((orig-buffer (current-buffer)))
    (with-current-buffer (switch-to-buffer (clone-indirect-buffer nil nil))
      (narrow-to-region beg end)
      (setq-local doom--narrowed-base-buffer orig-buffer))))

;;;###autoload
(defun doom/widen-indirectly-narrowed-buffer (&optional arg)
  "Widens narrowed buffers.

This command will incrementally kill indirect buffers (under the assumption they
were created by `doom/narrow-buffer-indirectly') and switch to their base
buffer.

If ARG, then kill all indirect buffers, return the base buffer and widen it.

If the current buffer is not an indirect buffer, it is `widen'ed."
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "Buffer isn't narrowed"))
  (let ((orig-buffer (current-buffer))
        (base-buffer doom--narrowed-base-buffer))
    (cond ((or (not base-buffer)
               (not (buffer-live-p base-buffer)))
           (widen))
          (arg
           (let ((buffer orig-buffer)
                 (buffers-to-kill (list orig-buffer)))
             (while (setq buffer (buffer-local-value 'doom--narrowed-base-buffer buffer))
               (push buffer buffers-to-kill))
             (switch-to-buffer (buffer-base-buffer))
             (mapc #'kill-buffer (remove (current-buffer) buffers-to-kill))))
          ((switch-to-buffer base-buffer)
           (kill-buffer orig-buffer)))))

;;;###autoload
(defun doom/toggle-narrow-buffer (beg end)
  "Narrow the buffer to BEG END. If narrowed, widen it."
  (interactive
   (list (or (bound-and-true-p evil-visual-beginning) (region-beginning))
         (or (bound-and-true-p evil-visual-end)       (region-end))))
  (if (buffer-narrowed-p)
      (widen)
    (unless (region-active-p)
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (narrow-to-region beg end)))
