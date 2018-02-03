;;; core/autoload/ui.el -*- lexical-binding: t; -*-

(defvar doom--line-number-style doom-line-numbers-style)
;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. The order depends on what
`doom-line-numbers-style' is set to.

Uses `display-line-numbers' in Emacs 26+ and `nlinum-mode' everywhere else."
  (interactive)
  (let* ((styles '(t relative nil))
         (order (cons doom-line-numbers-style (delq doom-line-numbers-style styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (cond ((boundp 'display-line-numbers)
           (when (and (eq next 'relative)
                      doom-line-numbers-visual-style)
             (setq next 'visual))
           (setq display-line-numbers next))
          ((featurep 'nlinum)
           (pcase next
             (`t (nlinum-relative-off) (nlinum-mode +1))
             (`relative (nlinum-relative-on))
             (`nil (nlinum-mode -1))))
          (t
           (error "No line number plugin detected")))
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`relative "relative")
               (`visual "visual")
               (`nil "disabled")))))

;;;###autoload
(defun doom-resize-window (window new-size &optional horizontal force-p)
  "Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise.
If FORCE-P is omitted when `window-size-fixed' is non-nil, resizing will fail."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
                      horizontal))))

;;;###autoload
(defun doom/window-zoom ()
  "Close other windows to focus on this one. Activate again to undo this. If the
window changes before then, the undo expires.

Alternatively, use `doom/window-enlargen'."
  (interactive)
  (if (and (one-window-p)
           (assoc ?_ register-alist))
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
                 (assoc ?_ register-alist))
            (ignore (jump-to-register ?_))
          (window-configuration-to-register ?_)
          (maximize-window)
          t)))

;;;###autoload
(defun doom/delete-frame ()
  "Delete the current frame, but ask for confirmation if it isn't empty."
  (interactive)
  (if (cdr (frame-list))
      (when (doom-quit-p "Close frame?")
        (delete-frame))
    (save-buffers-kill-emacs)))

;;;###autoload
(define-minor-mode doom-big-font-mode
  "A global mode that resizes the font, for streams, screen-sharing and
presentations."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless (fontp doom-big-font)
    (user-error "`doom-big-font' isn't set to a valid font"))
  (if doom-big-font-mode
      (set-frame-font doom-big-font t t)
    (set-frame-font doom-font t t)))

;;;###autoload
(defun doom//reload-theme ()
  "Reset the color theme currently in use."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) doom-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (run-hooks 'doom-pre-reload-theme-hook)
    (doom|init-ui)
    (run-hooks 'doom-post-reload-theme-hook)))
