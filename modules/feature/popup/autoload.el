;;; feature/popup/autoload.el -*- lexical-binding: t; -*-

(defvar +popup--populate-wparams (version< emacs-version "26.1"))

(defun +popup--remember (windows)
  "Remember WINDOWS (a list of windows) for later restoration."
  (cl-assert (cl-every #'windowp windows) t)
  (setq +popup--last
        (cl-loop for w in windows
                 collect (cons (window-buffer w)
                               (window-state-get w)))))

(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER, as was requested by a transient timer. If it fails, eg.
the buffer is visible, then set another timer and try again later."
  (when (buffer-live-p buffer)
    (let ((kill-buffer-hook (delq '+popup|kill-buffer-hook kill-buffer-hook)))
      (cond ((eq ttl 0)
             (kill-buffer buffer))
            ((get-buffer-window buffer)
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))
            (t
             (with-demoted-errors "Error killing transient buffer: %s"
               (when-let* ((process (get-buffer-process (current-buffer))))
                 (kill-process process))
               (kill-buffer buffer)))))))

(defun +popup--init (window alist)
  "Initializes a popup window. Run any time a popup is opened. It sets the
default window parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right)) t)
    (when +popup--populate-wparams
      ;; Emacs 26+ will automatically map the window-parameters alist entry to
      ;; the popup window, so we need this for Emacs 25.x users
      (dolist (param (cdr (assq 'window-parameters alist)))
        (set-window-parameter window (car param) (cdr param))))
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'no-other-window t)
    (set-window-parameter window 'delete-window #'+popup--destroy)
    (set-window-dedicated-p window 'popup)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))

(defun +popup--destroy (window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it get a chance to
  run and do cleanup of its own.
+ Either kills the buffer or sets a transient timer, if the window has a
  `transient' window parameter (see `+popup-window-parameters').
+ And finally deletes the window!"
  (let ((buffer (window-buffer window))
        ttl)
    (when (and (buffer-file-name buffer)
               (buffer-modified-p buffer))
      (with-current-buffer buffer
        (if (y-or-n-p "Popup buffer is modified. Save it?")
            (save-buffer)
          (set-buffer-modified-p nil))))
    (let ((ignore-window-parameters t))
      (delete-window window))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (+popup-buffer-mode -1)
        ;; t = default
        ;; integer = ttl
        ;; nil = no timer
        (unless +popup--inhibit-transient
          (setq ttl (+popup-parameter-fn 'transient window buffer))
          (when ttl
            (when (eq ttl t)
              (setq ttl +popup-ttl))
            (cl-assert (integerp ttl) t)
            (if (= ttl 0)
                (+popup--kill-buffer buffer 0)
              (add-hook 'kill-buffer-hook #'+popup|kill-buffer-hook nil t)
              (setq +popup--timer
                    (run-at-time ttl nil #'+popup--kill-buffer
                                 buffer ttl)))))))))

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters' with ALIST."
  (if (not alist)
      (setq alist +popup-default-alist)
    (let* ((alist  (map-merge 'list +popup-default-alist alist))
           (params (map-merge 'list
                              +popup-default-parameters
                              (cdr (assq 'window-parameters alist)))))
      (map-put alist 'window-parameters params)
      (nreverse alist))))


;;
;; Public library
;;

;;;###autoload
(defun +popup-buffer-p (&optional buffer)
  "Return t if BUFFER is a popup buffer. Defaults to the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (cl-assert (bufferp buffer) t)
  (and (buffer-live-p buffer)
       (buffer-local-value '+popup-buffer-mode buffer)
       buffer))

;;;###autoload
(defun +popup-window-p (&optional window)
  "Return t if WINDOW is a popup window. Defaults to the current window."
  (unless window
    (setq window (selected-window)))
  (cl-assert (windowp window) t)
  (and (window-live-p window)
       (window-parameter window 'popup)
       window))

;;;###autoload
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let ((old-window (selected-window))
        (alist (+popup--normalize-alist alist))
        (window-min-height 3))
    (when-let* ((new-window (run-hook-with-args-until-success
                             '+popup-display-buffer-actions buffer alist)))
      (+popup--init new-window alist)
      (let ((select (+popup-parameter 'select new-window)))
        (if (functionp select)
            (funcall select new-window old-window)
          (select-window (if select new-window old-window))))
      new-window)))

;;;###autoload
(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-parameter (or window (selected-window)) parameter))

;;;###autoload
(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a function, run it
with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

;;;###autoload
(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-window-p (window-list)))

;;;###autoload
(defun +popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.

Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))


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
         (add-hook 'doom-unreal-buffer-functions #'+popup-buffer-p)
         (add-hook 'doom-escape-hook #'+popup|close-on-escape t)
         (add-hook 'doom-cleanup-hook #'+popup|cleanup-rules)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (t
         (remove-hook 'doom-unreal-buffer-functions #'+popup-buffer-p)
         (remove-hook 'doom-escape-hook #'+popup|close-on-escape)
         (remove-hook 'doom-cleanup-hook #'+popup|cleanup-rules)
         (setq display-buffer-alist +popup--old-display-buffer-alist)
         (+popup|cleanup-rules)
         (dolist (prop +popup-window-parameters)
           (map-delete prop window-persistent-parameters)))))

;;;###autoload
(define-minor-mode +popup-buffer-mode
  "Minor mode for popup windows."
  :init-value nil
  :keymap +popup-buffer-mode-map
  (when (and +popup-buffer-mode (timerp +popup--timer))
    (remove-hook 'kill-buffer-hook #'+popup|kill-buffer-hook t)
    (cancel-timer +popup--timer)
    (setq +popup--timer nil)))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)


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
+ If nil (or omitted), then hide the modeline entirely (the default).
+ If a function, it takes the current buffer as its argument and must return one
  of the above values."
  (if +popup-buffer-mode
      (let ((modeline (+popup-parameter-fn 'modeline nil (current-buffer))))
        (cond ((eq modeline 't))
              ((or (eq modeline 'nil)
                   (not modeline))
               (doom-hide-modeline-mode +1))
              ((symbolp modeline)
               (when-let* ((doom--modeline-format (doom-modeline modeline)))
                 (doom-hide-modeline-mode +1)))))
    (when doom-hide-modeline-mode
      (doom-hide-modeline-mode -1))))

;;;###autoload
(defun +popup|close-on-escape ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If called outside, try to close all popup windows (see
`+popup/close-all')."
  (if (+popup-window-p)
      (+popup/close)
    (+popup/close-all)))

;;;###autoload
(defun +popup|cleanup-rules ()
  "Cleans up any duplicate popup rules."
  (interactive)
  (cl-delete-duplicates
   +popup--display-buffer-alist
   :key #'car :test #'equal :from-end t)
  (when +popup-mode
    (setq display-buffer-alist +popup--display-buffer-alist)))

;;;###autoload
(defun +popup|kill-buffer-hook ()
  "TODO"
  (let ((buf (current-buffer))
        (+popup--inhibit-transient t))
    (when (+popup-buffer-p buf)
      (when-let* ((window (get-buffer-window buf)))
        (when (+popup-window-p window)
          (+popup--destroy window))))))


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
    (select-window (if (+popup-window-p)
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
  (when (and (+popup-window-p window)
             (or force-p
                 (memq (+popup-parameter-fn 'quit window window)
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
                (memq (+popup-parameter-fn 'quit window window)
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
  (let ((+popup--inhibit-transient t))
    (cond ((+popup-windows)
           (+popup/close-all t))
          ((ignore-errors (+popup/restore)))
          ((display-buffer (get-buffer "*Messages*"))))))

;;;###autoload
(defun +popup/restore ()
  "Restore the last popups that were closed, if any."
  (interactive)
  (unless +popup--last
    (error "No popups to restore"))
  (cl-loop for (buffer . state) in +popup--last
           if (and (buffer-live-p buffer)
                   (display-buffer buffer))
           do (window-state-put state it))
  (setq +popup--last nil))

;;;###autoload
(defun +popup/raise ()
  "Raise the current popup window into a regular window."
  (interactive)
  (unless (+popup-window-p)
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
  `(let* ((in-popup-p (+popup-buffer-p))
          (popups (+popup-windows))
          (+popup--inhibit-transient t)
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


;;
;; Advice
;;

;;;###autoload
(defun +popup*close (&rest _)
  "TODO"
  (+popup/close))

;;;###autoload
(defun +popup*save (orig-fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (save-popups! (apply orig-fn args)))


;;
;; Popup actions
;;

(defun +popup--dimension (side)
  (if (memq side '(left right))
      'window-width
    'window-height))

(defun +popup--side (side)
  (pcase side (`bottom 'below) (`top 'above) (_ side)))

(defun +popup--frame-splittable-p (frame)
  (when (and (window--frame-usable-p frame)
             (not (frame-parameter frame 'unsplittable)))
    frame))

(defun +popup--splittable-frame ()
  (let ((selected-frame (selected-frame))
        (last-non-minibuffer-frame (last-nonminibuffer-frame)))
    (or (+popup--frame-splittable-p selected-frame)
        (+popup--frame-splittable-p last-non-minibuffer-frame))))

;;;###autoload
(defun +popup-display-buffer (buf alist)
  "Highly experimental!"
  (when-let* ((frame (+popup--splittable-frame)))
    (let* ((-side (or (alist-get 'side alist) 'bottom))
           (side (+popup--side -side))
           (size (alist-get (+popup--dimension -side) alist))
           (old-size (window-size (frame-root-window frame)
                                  (memq -side '(left right))))
           (new-size
            (when (numberp size)
              (round (if (>= size 1)
                         (- old-size size)
                       (* (- 1 size) old-size)))))
           (window (split-window (frame-root-window frame) new-size side)))
      (window--display-buffer buf window 'window alist t))))
