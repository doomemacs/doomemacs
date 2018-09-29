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
(defun doom/reload-theme ()
  "Reset the current color theme and fonts."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) doom-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (doom|init-theme)
    (doom|init-fonts)))

;;;###autoload
(defun doom/delete-frame ()
  "Delete the current frame, but ask for confirmation if it isn't empty."
  (interactive)
  (if (cdr (frame-list))
      (when (doom-quit-p "Close frame?")
        (delete-frame))
    (save-buffers-kill-emacs)))

;;;###autoload
(defun doom/window-zoom ()
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
windows (unlike `doom/window-zoom') Activate again to undo."
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
(defun doom/reload-font ()
  "Reload `doom-font', `doom-variable-pitch-font', and `doom-unicode-font', if
set."
  (interactive)
  (when doom-font
    (set-frame-font doom-font t))
  (doom|init-fonts))

;;;###autoload
(defun doom/set-frame-opacity (opacity)
  "Interactively change the current frame's opacity.

OPACITY is an integer between 0 to 100, inclusive."
  (interactive
   (list (read-number "Opacity (0-100): "
                      (or (frame-parameter nil 'alpha)
                          100))))
  (set-frame-parameter nil 'alpha opacity))


;;
;; Modes

;;;###autoload
(define-minor-mode doom-big-font-mode
  "A global mode that resizes the font, for streams, screen-sharing and
presentations.

Uses `doom-big-font' when enabled."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless doom-big-font
    (user-error "`doom-big-font' must be set to a valid font"))
  (unless doom-font
    (user-error "`doom-font' must be set to a valid font"))
  (set-frame-font (if doom-big-font-mode
                      doom-big-font
                    doom-font)
                  t t))
