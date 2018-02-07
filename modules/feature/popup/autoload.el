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

(defun +popup--init (window &optional alist)
  "Initializes a popup window. Run any time a popup is opened. It sets the
default window parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (when (and alist +popup--populate-wparams)
      ;; Emacs 26+ will automatically map the window-parameters alist entry to
      ;; the popup window, so we need this for Emacs 25.x users
      (dolist (param (cdr (assq 'window-parameters alist)))
        (set-window-parameter window (car param) (cdr param))))
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'no-other-window t)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup/close-all)
    (set-window-dedicated-p window 'popup)
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right))
     t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))

(defun +popup--delete-window (window)
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
               display-buffer-alist +popup--display-buffer-alist
               window--sides-inhibit-check t)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (t
         (remove-hook 'doom-unreal-buffer-functions #'+popup-buffer-p)
         (remove-hook 'doom-escape-hook #'+popup|close-on-escape)
         (remove-hook 'doom-cleanup-hook #'+popup|cleanup-rules)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               window--sides-inhibit-check nil)
         (+popup|cleanup-rules)
         (dolist (prop +popup-window-parameters)
           (map-delete window-persistent-parameters prop)))))

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
  (when-let* ((window (get-buffer-window)))
    (when (+popup-window-p window)
      (let ((+popup--inhibit-transient t))
        (+popup--delete-window window)))))


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
    (cond ((+popup-windows) (+popup/close-all t))
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
  (setq +popup--last nil)
  t)

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
  (+popup/close nil t))

;;;###autoload
(defun +popup*save (orig-fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (save-popups! (apply orig-fn args)))


;;
;; Popup actions
;;

(when (version< emacs-version "26")
  (defvar window-sides-reversed nil)

  (defun window--sides-reverse-on-frame-p (frame)
    "Return non-nil when side windows should appear reversed on FRAME.
This uses some heuristics to guess the user's intentions when the
selected window of FRAME is a side window ."
    (cond
     ;; Reverse when `window-sides-reversed' is t.  Do not reverse when
     ;; `window-sides-reversed' is nil.
     ((memq window-sides-reversed '(nil t))
      window-sides-reversed)
     ;; Reverse when FRAME's selected window shows a right-to-left buffer.
     ((let ((window (frame-selected-window frame)))
        (when (and (not (window-parameter window 'window-side))
                   (or (not (window-minibuffer-p window))
                       (setq window (minibuffer-selected-window))))
          (with-current-buffer (window-buffer window)
            (eq bidi-paragraph-direction 'right-to-left)))))
     ;; Reverse when FRAME's `window-sides-main-selected-window' parameter
     ;; specifies a live window showing a right-to-left buffer.
     ((let ((window (frame-parameter
                     frame 'window-sides-main-selected-window)))
        (when (window-live-p window)
          (with-current-buffer (window-buffer window)
            (eq bidi-paragraph-direction 'right-to-left)))))
     ;; Reverse when all windows in FRAME's main window show right-to-left
     ;; buffers.
     (t
      (catch 'found
        (walk-window-subtree
         (lambda (window)
           (with-current-buffer (window-buffer window)
             (when (eq bidi-paragraph-direction 'left-to-right)
               (throw 'found nil))))
         (window-main-window frame))
        t))))

  (defun window--make-major-side-window (buffer side slot &optional alist)
    "Display BUFFER in a new major side window on the selected frame.
SIDE must be one of `left', `top', `right' or `bottom'.  SLOT
specifies the slot to use.  ALIST is an association list of
symbols and values as passed to `display-buffer-in-side-window'.
Return the new window, nil if its creation failed.

This is an auxiliary function of `display-buffer-in-side-window'
and may be called only if no window on SIDE exists yet."
    (let* ((left-or-right (memq side '(left right)))
           (next-to (window--make-major-side-window-next-to side))
           (on-side (cond
                     ((eq side 'top) 'above)
                     ((eq side 'bottom) 'below)
                     (t side)))
           (window--sides-inhibit-check t)
           ;; The following two bindings will tell `split-window' to take
           ;; the space for the new window from the selected frame's main
           ;; window and not make a new parent window unless needed.
           (window-combination-resize 'side)
           (window-combination-limit nil)
           (window (ignore-errors (split-window next-to nil on-side))))
      (when window
        ;; Initialize `window-side' parameter of new window to SIDE and
        ;; make that parameter persistent.
        (set-window-parameter window 'window-side side)
        (add-to-list 'window-persistent-parameters '(window-side . writable))
        ;; Install `window-slot' parameter of new window and make that
        ;; parameter persistent.
        (set-window-parameter window 'window-slot slot)
        (add-to-list 'window-persistent-parameters '(window-slot . writable))
        ;; Auto-adjust height/width of new window unless a size has been
        ;; explicitly requested.
        (unless (if left-or-right
                    (cdr (assq 'window-width alist))
                  (cdr (assq 'window-height alist)))
          (setq alist
                (cons
                 (cons
                  (if left-or-right 'window-width 'window-height)
                  (/ (window-total-size (frame-root-window) left-or-right)
                     ;; By default use a fourth of the size of the frame's
                     ;; root window.
                     4))
                 alist)))
        (with-current-buffer buffer
          (setq window--sides-shown t))
        ;; Install BUFFER in new window and return WINDOW.
        (window--display-buffer buffer window 'window alist 'side))))

  (advice-add #'window--sides-check :override #'ignore))

;;;###autoload
(defun +popup-display-buffer (buffer alist)
  "A `display-buffer' action that serves as an alternative to
`display-buffer-in-side-window', but allows for stacking popups not only
laterally with the `vslot' alist entry.

Accepts the same arguments as `display-buffer-in-side-window'. You must set
`window--sides-inhibit-check' to non-nil for this work properly."
  (let* ((side  (or (cdr (assq 'side alist)) 'bottom))
         (slot  (or (cdr (assq 'slot alist))  0))
         (vslot (or (cdr (assq 'vslot alist)) 0))
         (left-or-right (memq side '(left right)))
         (dedicated (or display-buffer-mark-dedicated 'popup)))

    (cond ((not (memq side '(top bottom left right)))
           (error "Invalid side %s specified" side))
          ((not (numberp slot))
           (error "Invalid slot %s specified" slot))
          ((not (numberp vslot))
           (error "Invalid vslot %s specified" vslot)))

    (let* ((major (get-window-with-predicate
                   (lambda (window)
                     (and (eq (window-parameter window 'window-side) side)
                          (eq (window-parameter window 'window-vslot) vslot)))
                   nil t))
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond ((window-live-p major)
                   (list major))
                  ((window-valid-p major)
                   (let* ((first (window-child major))
                          (next (window-next-sibling first))
                          (windows (list next first)))
                     (setq reversed (> (window-parameter first 'window-slot)
                                       (window-parameter next 'window-slot)))
                     (while (setq next (window-next-sibling next))
                       (setq windows (cons next windows)))
                     (if reversed windows (nreverse windows))))))
           (slots (if major (max 1 (window-child-count major))))
           (max-slots
            (nth (plist-get '(left 0 top 1 right 2 bottom 3) side)
                 window-sides-slots))
           (window--sides-inhibit-check t)
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)

      (cond ((and (numberp max-slots) (<= max-slots 0))
             nil)
            ((not windows)
             (cl-letf (((symbol-function 'window--make-major-side-window-next-to)
                        (lambda (_side) (frame-root-window (selected-frame)))))
               (when-let* ((window (window--make-major-side-window buffer side slot alist)))
                 (set-window-parameter window 'window-vslot vslot)
                 (add-to-list 'window-persistent-parameters '(window-vslot . writable))
                 window)))
            (t
             ;; Scan windows on SIDE.
             (catch 'found
               (dolist (window windows)
                 (setq this-slot (window-parameter window 'window-slot))
                 (cond ((not (numberp this-slot)))
                       ((= this-slot slot) ; A window with a matching slot found
                        (setq this-window window)
                        (throw 'found t))
                       (t
                        ;; Check if this window has a better slot value wrt the
                        ;; slot of the window we want.
                        (setq abs-slot
                              (if (or (and (> this-slot 0) (> slot 0))
                                      (and (< this-slot 0) (< slot 0)))
                                  (abs (- slot this-slot))
                                (+ (abs slot) (abs this-slot))))
                        (unless (and best-slot (<= best-slot abs-slot))
                          (setq best-window window)
                          (setq best-slot abs-slot))
                        (if reversed
                            (cond
                             ((<= this-slot slot)
                              (setq next-window window))
                             ((not prev-window)
                              (setq prev-window window)))
                          (cond
                           ((<= this-slot slot)
                            (setq prev-window window))
                           ((not next-window)
                            (setq next-window window))))))))

             ;; `this-window' is the first window with the same SLOT.
             ;; `prev-window' is the window with the largest slot < SLOT. A new
             ;; window will be created after it.
             ;; `next-window' is the window with the smallest slot > SLOT. A new
             ;; window will be created before it.
             ;; `best-window' is the window with the smallest absolute
             ;; difference of its slot and SLOT.
             (or (and this-window
                      ;; Reuse `this-window'.
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer this-window 'reuse alist dedicated))
                 (and (or (not max-slots) (< slots max-slots))
                      (or (and next-window
                               ;; Make new window before `next-window'.
                               (let ((next-side (if left-or-right 'above 'left))
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window next-window nil next-side)))))
                          (and prev-window
                               ;; Make new window after `prev-window'.
                               (let ((prev-side (if left-or-right 'below 'right))
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window prev-window nil prev-side))))))
                      (set-window-parameter window 'window-slot slot)
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer window 'window alist dedicated))
                 (and best-window
                      ;; Reuse `best-window'.
                      (progn
                        ;; Give best-window the new slot value.
                        (set-window-parameter best-window 'window-slot slot)
                        (with-current-buffer buffer
                          (setq window--sides-shown t))
                        (window--display-buffer
                         buffer best-window 'reuse alist dedicated)))))))))
