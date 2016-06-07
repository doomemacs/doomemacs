;;; defuns-popups.el

(defvar doom-last-popup nil
  "The last popup buffer open (or group thereof).")

(defvar-local doom-popup-rule nil
  "A list of rules applied to this popup.")

(defvar doom-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap doom/kill-real-buffer] 'doom/popup-close)
    (define-key map [remap evil-window-move-very-bottom] 'ignore)
    (define-key map [remap evil-window-move-very-top]    'ignore)
    (define-key map [remap evil-window-move-far-left]    'ignore)
    (define-key map [remap evil-window-move-far-right]   'ignore)
    (define-key map [remap evil-window-split]  'ignore)
    (define-key map [remap evil-window-vsplit] 'ignore)
    map)
   "Active keymap in popup windows.")

(advice-add 'doom/evil-window-move :around 'doom*popup-window-move)

;;;###autoload
(defun doom*popup-window-move (orig-fn &rest args)
  (unless (doom/popup-p)
    (apply orig-fn args)))

;;;###autoload
(defun doom/popup-p (&optional window)
  "Whether WINDOW is a shackle popup window or not. If WINDOW is nil, use
current window."
  (let ((window (or window (selected-window))))
    (and (window-live-p window)
         (with-selected-window window (bound-and-true-p doom-popup-mode))
         window)))

;;;###autoload
(defun doom/popups-p ()
  "Whether there is a popup window open and alive somewhere."
  (and doom-last-popup
       (window-live-p (get-buffer-window doom-last-popup))))

;;;###autoload
(defmacro doom/popup-save (&rest body)
  "Close popups before BODY and restore them afterwards."
  `(let ((popup-p (doom/popups-p))
         (in-popup-p (doom/popup-p)))
     (when popup-p
       (doom/popup-close-all t t))
     (prog1
         ,@body
       (when popup-p
         (if in-popup-p
             (doom/popup-last-buffer)
           (save-excursion (doom/popup-last-buffer)))))))

;;;###autoload
(defun doom/popup-buffer (buffer &optional plist)
  "Display BUFFER in a shackle popup."
  (let ((buffer-name (cond ((stringp buffer) buffer)
                           ((bufferp buffer) (buffer-name buffer))
                           (t (error "Not a valid buffer")))))
    (shackle-display-buffer
     (get-buffer-create buffer-name)
     nil (or plist (shackle-match buffer-name)))))

;;;###autoload
(defun doom/popup-close (&optional window dont-kill dont-redraw)
  "Find and close the currently active popup (if available)."
  (interactive)
  (setq window (or window (selected-window)))
  (when (doom/popup-p window)
    (with-selected-window window
      ;; If REPL...
      (when (bound-and-true-p repl-toggle-mode)
        (setq rtog/--last-buffer nil))
      (if (not (or dont-kill (memq :nokill doom-popup-rule)))
          (let ((kill-buffer-query-functions
                 (delq 'process-kill-buffer-query-function
                       kill-buffer-query-functions)))
            (kill-buffer (window-buffer window)))
        (doom-popup-mode -1))
      (delete-window window)
      (when (< emacs-major-version 25)
        (unless dont-redraw (redraw-frame))))))

;;;###autoload
(defun doom/popup-close-all (&optional dont-kill dont-redraw)
  "Closes all popups (kill them if DONT-KILL-BUFFERS is non-nil). Then redraw
the display (unless DONT-REDRAW is non-nil)."
  (interactive)
  (mapc (lambda (w) (doom/popup-close w dont-kill t))
        (doom/get-visible-windows (buffer-list)))
  (when (< emacs-major-version 25)
    (unless dont-redraw (redraw-frame))))

;;;###autoload
(defun doom/popup-last-buffer ()
  "Restore the last popup."
  (interactive)
  (unless (buffer-live-p doom-last-popup)
    (setq doom-last-popup nil)
    (error "No popup to restore"))
  (doom/popup-buffer doom-last-popup))

;;;###autoload
(defun doom/popup-messages ()
  "Pop up the messages buffer."
  (interactive)
  (doom/popup-buffer (messages-buffer))
  (goto-char (point-max)))

;;;###autoload
(defun doom*popup-init (orig-fn &rest args)
  "Enable `doom-popup-mode' in every popup window and returns the window."
  (let ((window (apply orig-fn args)))
    (with-current-buffer (window-buffer window)
      (doom-popup-mode +1))
    ;; NOTE orig-fn returns a window, so `doom*popup-init' must too
    window))

;;;###autoload
(defun doom*save-popup (orig-fun &rest args)
  "Prevents messing up a popup buffer on window changes"
  (doom/popup-save (apply orig-fun args)))

(put 'doom-popup-mode 'permanent-local t)
(put 'doom-popup-mode-map 'permanent-local t)

;;;###autoload
(define-minor-mode doom-popup-mode
  "Pop ups"
  :init-value nil
  :keymap doom-popup-mode-map
  :global nil
  (let ((rules (or doom-popup-rule
                   (--any (let ((key (car it)))
                            (when (cond ((symbolp key)
                                         (or (eq major-mode key)
                                             (derived-mode-p key)))
                                        ((and (stringp key) buffer-file-name)
                                         (string-match-p key buffer-file-name)))
                              (cdr it)))
                          doom-popup-rules))))
    (setq doom-last-popup (current-buffer))
    (setq-local doom-popup-rule rules)
    ;; (set-window-dedicated-p (selected-window) doom-popup-mode)
    (unless (memq :noesc rules)
      (make-local-variable 'doom-popup-mode-map)
      (let ((map doom-popup-mode-map))
        (define-key map [remap evil-force-normal-state] 'doom/popup-close)
        (define-key map [escape] 'doom/popup-close)
        (define-key map (kbd "ESC") 'doom/popup-close)))
    (unless (memq :modeline rules)
      (doom-hide-mode-line-mode (if doom-popup-mode +1 -1)))))

(provide 'defuns-popups)
;;; defuns-popups.el ends here
