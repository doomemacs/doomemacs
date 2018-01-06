;;; feature/popup/autoload.el -*- lexical-binding: t; -*-

(defun +popup--cancel-buffer-timer ()
  "Cancel the current buffer's transient timer."
  (when (timerp +popup--timer)
    (message "Cancelled timer")
    (cancel-timer +popup--timer)
    (setq +popup--timer nil))
  t)

(defun +popup--remember (windows)
  "Remember WINDOWS (a list of windows) for later restoration."
  (cl-assert (cl-every #'windowp windows) t)
  (setq +popup--last
        (cl-loop for w in windows
                 collect (list (window-buffer w)
                               (window-parameter w 'alist)
                               (window-state-get w)))))

(defun +popup--kill-buffer (buffer)
  "Tries to kill BUFFER, as was requested by a transient timer. If it fails, eg.
the buffer is visible, then set another timer and try again later."
  (when (buffer-live-p buffer)
    (if (get-buffer-window buffer)
        (with-current-buffer buffer
          (setq +popup--timer
                (run-at-time (timer--time +popup--timer)
                             nil #'+popup--kill-buffer buffer)))
      (with-demoted-errors "Error killing transient buffer: %s"
        (let ((inhibit-message (not doom-debug-mode)))
          (message "Cleaned up transient buffer: %s" buffer))
        (when-let* ((process (get-buffer-process (current-buffer))))
          (kill-process process))
        (kill-buffer buffer)))))

(defun +popup--init (window alist)
  "Initializes a popup window. Run any time a popup is opened. It sets the
default window parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (set-window-parameter window 'no-other-window t)
    (set-window-parameter window 'delete-window #'+popup--destroy)
    (set-window-parameter window 'alist alist)
    (window-preserve-size
     window (memq (window-parameter window 'window-side) '(left right)) t)
    (+popup--cancel-buffer-timer)
    (+popup-buffer-mode +1)))

(defun +popup--destroy (window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it get a chance to
  run and do cleanup of its own.
+ Either kills the buffer or sets a transient timer, if the window has a
  `transient' window parameter (see `+popup-window-parameters').
+ And finally deletes the window!"
  (let ((ttl (+popup-parameter 'transient window))
        (buffer (window-buffer window)))
    (let ((ignore-window-parameters t))
      (delete-window window))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (+popup-buffer-mode -1)
        ;; t = default
        ;; integer = ttl
        ;; nil = no timer
        (when ttl
          (when (eq ttl t)
            (setq ttl +popup-ttl))
          (cl-assert (integerp ttl) t)
          (if (= ttl 0)
              (+popup--kill-buffer buffer)
            (setq +popup--timer
                  (run-at-time ttl nil #'+popup--kill-buffer buffer))))))))

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters' with ALIST."
  (if (not alist)
      (setq alist +popup-default-alist)
    (require 'map)
    (let* ((alist  (map-merge 'list +popup-default-alist alist))
           (params (map-merge 'list
                              +popup-default-parameters
                              (cdr (assq 'window-parameters alist)))))
      (setq alist (assq-delete-all 'window-parameters alist))
      (push (cons 'window-parameters params) alist)
      (nreverse alist))))


;;
;; Public library
;;

;;;###autoload
(defun +popup-p (&optional target)
  "Return t if TARGET is a popup window or buffer. If TARGET is nil, use the
current buffer."
  (unless target
    (setq target (current-buffer)))
  (cond ((windowp target)
         (+popup-p (window-buffer target)))
        ((bufferp target)
         (buffer-local-value '+popup-buffer-mode target))
        (t
         (error "Expected a window/buffer, got %s (%s)"
                (type-of target) target))))

;;;###autoload
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let* ((old-window (selected-window))
         (alist (+popup--normalize-alist alist))
         (new-window (or (display-buffer-reuse-window buffer alist)
                         (display-buffer-in-side-window buffer alist))))
    (+popup--init new-window alist)
    (select-window
     (if (+popup-parameter 'select new-window)
         new-window
       old-window))
    new-window))

;;;###autoload
(defun +popup-parameter (parameter &optional window)
  "Fetch the window parameter of WINDOW"
  (window-parameter (or window (selected-window)) parameter))

;;;###autoload
(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-p (window-list)))


;;
;; Minor mode
;;

;;;###autoload
(define-minor-mode +popup-mode
  "Global minor mode for popups."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  (cond (+popup-mode
         (add-hook 'doom-unreal-buffer-functions #'+popup-p)
         (add-hook 'doom-escape-hook #'+popup|close-on-escape t)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (t
         (remove-hook 'doom-unreal-buffer-functions #'+popup-p)
         (remove-hook 'doom-escape-hook #'+popup|close-on-escape)
         (setq display-buffer-alist +popup--old-display-buffer-alist)
         (dolist (prop +popup-window-parameters)
           (assq-delete-all prop window-persistent-parameters)))))

;;;###autoload
(define-minor-mode +popup-buffer-mode
  "Minor mode for popup windows."
  :init-value nil
  :keymap +popup-buffer-mode-map)


;;
;; Hooks
;;

;;;###autoload
(defun +popup|adjust-fringes ()
  "Hides the fringe in popup windows, restoring them if `+popup-buffer-mode' is
disabled."
  (let ((f (if +popup-buffer-mode 0 doom-fringe-size)))
    (set-window-fringes nil f f fringes-outside-margins)))

;;;###autoload
(defun +popup|set-modeline ()
  "Don't show modeline in popup windows without a `modeline' window-parameter.

+ If one exists and it's a symbol, use `doom-modeline' to grab the format.
+ If non-nil, show the mode-line as normal.
+ If nil (or omitted), then hide the modeline entirely (the default)."
  (if +popup-buffer-mode
      (let ((modeline (+popup-parameter 'modeline)))
        (cond ((or (eq modeline 'nil)
                   (not modeline))
               (doom-hide-modeline-mode +1))
              ((and (symbolp modeline)
                    (not (eq modeline 't)))
               (setq-local doom--modeline-format (doom-modeline modeline))
               (when doom--modeline-format
                 (doom-hide-modeline-mode +1)))))
    (when doom-hide-modeline-mode
      (doom-hide-modeline-mode -1))))

;;;###autoload
(defun +popup|close-on-escape ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If called outside, try to close all popup windows (see
`+popup/close-all')."
  (call-interactively
   (if (+popup-p)
       #'+popup/close
     #'+popup/close-all)))


;;
;; Commands
;;

;;;###autoload
(defalias 'other-popup #'+popup/other)

;;;###autoload
(defun +popup/other ()
  "Cycle through popup windows, like `other-window'. Ignores regular windows."
  (interactive)
  (let ((popups (+popup-windows))
        (window (selected-window)))
    (unless popups
      (user-error "No popups are open"))
    (select-window (if (+popup-p)
                       (or (car-safe (cdr (memq window popups)))
                           (car (delq window popups))
                           (car popups))
                     (car popups)))))

;;;###autoload
(defun +popup/close (&optional window force-p)
  "Close WINDOW, if it's a popup window.

This will do nothing if the popup's `quit' window parameter is either nil or
'other. This window parameter is ignored if FORCE-P is non-nil."
  (interactive
   (list (selected-window)
         current-prefix-arg))
  (unless window
    (setq window (selected-window)))
  (when (and (+popup-p window)
             (or force-p
                 (memq (+popup-parameter 'quit window)
                       '(t current))))
    (when +popup--remember-last
      (+popup--remember (list window)))
    (delete-window window)
    t))

;;;###autoload
(defun +popup/close-all (&optional force-p)
  "Close all open popup windows.

This will ignore popups with an `quit' parameter that is either nil or 'current.
This window parameter is ignored if FORCE-P is non-nil."
  (interactive "P")
  (let (targets +popup--remember-last)
    (dolist (window (+popup-windows))
      (when (or force-p
                (memq (+popup-parameter 'quit window)
                      '(t other)))
        (push window targets)))
    (when targets
      (+popup--remember targets)
      (mapc #'delete-window targets)
      t)))

;;;###autoload
(defun +popup/toggle ()
  "If popups are open, close them. If they aren't, restore the last one or open
the message buffer in a popup window."
  (interactive)
  (cond ((+popup-windows)
         (+popup/close-all))
        ((ignore-errors (+popup/restore)))
        ((display-buffer (get-buffer "*Messages*")))))

;;;###autoload
(defun +popup/restore ()
  "Restore the last popups that were closed, if any."
  (interactive)
  (unless +popup--last
    (error "No popups to restore"))
  (cl-loop for (buffer alist state) in +popup--last
           if (and (buffer-live-p buffer)
                   (+popup-buffer buffer alist))
           do (window-state-put state it))
  (setq +popup--last nil))

;;;###autoload
(defun +popup/raise ()
  "Raise the current popup window into a regular window."
  (interactive)
  (unless (+popup-p)
    (user-error "Cannot raise a non-popup window"))
  (let ((window (selected-window))
        (buffer (current-buffer))
        +popup--remember-last)
    (set-window-parameter window 'transient nil)
    (+popup/close window 'force)
    (display-buffer-pop-up-window buffer nil)))


;;
;; Macros
;;

;;;###autoload
(defmacro without-popups! (&rest body)
  "Run BODY with a default `display-buffer-alist', ignoring the popup rules set
with the :popup setting."
  `(let ((display-buffer-alist +popup--old-display-buffer-alist))
     ,@body))

;;;###autoload
(defmacro save-popups! (&rest body)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  `(let* ((in-popup-p (+popup-p))
          (popups (+popup-windows))
          +popup--last)
     (dolist (p popups)
       (+popup/close p 'force))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (+popup/restore)
           (unless in-popup-p
             (select-window origin)))))))

