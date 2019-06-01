;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;;
;; Public library

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
;; Advice

;;;###autoload
(defun doom*recenter (&rest _)
  "Generic advisor for recentering window (typically :after other functions)."
  (recenter))

;;;###autoload
(defun doom*shut-up (orig-fn &rest args)
  "Generic advisor for silencing noisy functions."
  (quiet! (apply orig-fn args)))


;;
;; Hooks

;;;###autoload
(defun doom|apply-ansi-color-to-compilation-buffer ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))


;;
;; Commands

;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.

See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if (and EMACS26+ visual-line-mode) 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (if EMACS26+
        (setq display-line-numbers next)
      (pcase next
        (`t (nlinum-relative-off) (nlinum-mode +1))
        (`relative (nlinum-relative-on))
        (`nil (nlinum-mode -1))))
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

;;;###autoload
(defun doom/window-maximize-buffer ()
  "Close other windows to focus on this one. Activate again to undo this. If the
window changes before then, the undo expires.

Alternatively, use `doom/window-enlargen'."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defvar doom--window-enlargened nil)
;;;###autoload
(defun doom/window-enlargen ()
  "Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-maximize-buffer') Activate again to undo."
  (interactive)
  (setq doom--window-enlargened
        (if (and doom--window-enlargened
                 (assq ?_ register-alist))
            (ignore (ignore-errors (jump-to-register ?_)))
          (window-configuration-to-register ?_)
          (if (window-dedicated-p)
              ;; `window-resize' and `window-max-delta' don't respect
              ;; `ignore-window-parameters', so we gotta force it to.
              (cl-letf* ((old-window-resize (symbol-function #'window-resize))
                         (old-window-max-delta (symbol-function #'window-max-delta))
                         ((symbol-function #'window-resize)
                          (lambda (window delta &optional horizontal _ignore pixelwise)
                            (funcall old-window-resize window delta horizontal
                                     t pixelwise)))
                         ((symbol-function #'window-max-delta)
                          (lambda (&optional window horizontal _ignore trail noup nodown pixelwise)
                            (funcall old-window-max-delta window horizontal t
                                     trail noup nodown pixelwise))))
                (maximize-window))
            (maximize-window))
          t)))

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

(defvar-local doom--buffer-narrowed-origin nil)
;;;###autoload
(defun doom/clone-and-narrow-buffer (beg end &optional clone-p)
  "Restrict editing in this buffer to the current region, indirectly. With CLONE-P,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "rP")
  (cond ((or (region-active-p)
             (and beg end))
         (deactivate-mark)
         (when clone-p
           (let ((old-buf (current-buffer)))
             (switch-to-buffer (clone-indirect-buffer nil nil))
             (setq doom--buffer-narrowed-origin old-buf)))
         (narrow-to-region beg end))
        (doom--buffer-narrowed-origin
         (kill-current-buffer)
         (switch-to-buffer doom--buffer-narrowed-origin)
         (setq doom--buffer-narrowed-origin nil))
        (t
         (widen))))
